#include "TypeDependencyCollector.h"
#include <iostream>
#include <ranges>
#include <Utils/TextWriter.h>

TypeDependency::TypeDependency(const CComPtr<IDiaSymbol>& InDependencySymbol) : DependencySymbol(InDependencySymbol)
{
    // Make sure to always grab the unmodified copy of the symbol when it is available, to avoid duplicates with different CV modifiers
    if ( const CComPtr<IDiaSymbol> UnmodifiedTypeSymbol = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( DependencySymbol, unmodifiedType ) )
    {
        DependencySymbol = UnmodifiedTypeSymbol;
    }
    SymbolTag = GET_SYMBOL_ATTRIBUTE_CHECKED( DependencySymbol, symTag );
    SymbolId = GET_SYMBOL_ATTRIBUTE_CHECKED( DependencySymbol, symIndexId );
}

TypeDependency TypeDependency::UserDefinedType(const CComPtr<IDiaSymbol>& UserDefinedType)
{
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( UserDefinedType, symTag ) == SymTagUDT );
    return TypeDependency{UserDefinedType};
}

TypeDependency TypeDependency::Enum(const CComPtr<IDiaSymbol>& Enum)
{
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( Enum, symTag ) == SymTagEnum );
    return TypeDependency{Enum};
}

CComPtr<IDiaSymbol> ITypeResolutionProvider::FindTopLevelType(const CComPtr<IDiaSymbol>& InNestedType)
{
    CComPtr<IDiaSymbol> CurrentSymbol = InNestedType;
    while ( const CComPtr<IDiaSymbol> NewParentSymbol = FindNestedTypeParent( CurrentSymbol ) )
    {
        CurrentSymbol = NewParentSymbol;
    }
    return CurrentSymbol;
}

TypeDependencyCollectorBase::TypeDependencyCollectorBase(ITypeResolutionProvider* InTypeResolver) : TypeResolver( InTypeResolver )
{
}

void TypeDependencyCollectorBase::CollectDependenciesForType( const CComPtr<IDiaSymbol>& InTypeSymbol, const bool bOnlyWantsPredeclaration )
{
    const DWORD SymbolTag = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, symTag );
    if ( SymbolTag == SymTagArrayType )
    {
        if ( const CComPtr<IDiaSymbol> ElementType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, type ) )
        {
            // Also always want a definition for the array index type, which is normally one of the base types
            CollectDependenciesForType( ElementType, false );
        }
        if ( const CComPtr<IDiaSymbol> IndexType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, arrayIndexType ) )
        {
            // We always want full type definitions for array element types to know how much memory to allocate for them
            CollectDependenciesForType( IndexType, false );
        }
    }
    else if ( SymbolTag == SymTagBaseType )
    {
        // Base types cannot be user defined, no dependencies for them other than to declare, but we need to include <cstdint> for them
        bNeedsCStdint = true;
    }
    else if ( SymbolTag == SymTagCustomType )
    {
        // We do not really support custom types, but might handle them for the sake of completeness
        DWORD NumCustomTypeDependencies = 0;
        assert( InTypeSymbol->get_types( 0, &NumCustomTypeDependencies, nullptr ) == S_OK );

        // This code only works if CComPtr internally is just a raw COM pointer
        std::vector<CComPtr<IDiaSymbol>> CustomTypes;
        static_assert(sizeof(CComPtr<IDiaSymbol>) == sizeof(IDiaSymbol*));

        CustomTypes.resize( NumCustomTypeDependencies );
        assert( InTypeSymbol->get_types( NumCustomTypeDependencies, &NumCustomTypeDependencies, (IDiaSymbol**) CustomTypes.data() ) == S_OK );

        for ( const CComPtr<IDiaSymbol>& ReferencedType : CustomTypes )
        {
            // We always want definitions for custom types, since we have no idea how they use underlying types
            CollectDependenciesForType( ReferencedType, false );
        }
    }
    else if ( SymbolTag == SymTagEnum )
    {
        // We never want definitions for enum types, just the pre-declaration
        AddDependency( TypeDependency::Enum( InTypeSymbol ), true );

        // We need a definition for the underlying enum type, even for a pre-declaration
        // TODO this is technically an implementation detail and maybe should be handled by AddDependency internally instead
        if ( const CComPtr<IDiaSymbol> UnderlyingType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, type ) )
        {
            CollectDependenciesForType( UnderlyingType, false );
        }
    }
    else if ( SymbolTag == SymTagPointerType )
    {
        // Pointers do not need the full definition of their pointee type, just the pre-declaration
        if ( const CComPtr<IDiaSymbol> PointeeType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, type ) )
        {
            // We always want full type definitions for array element types to know how much memory to allocate for them
            CollectDependenciesForType( PointeeType, true );
        }
    }
    else if ( SymbolTag == SymTagTypedef )
    {
        const std::wstring TypedefName = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, name );

        // For typedefs, we want a full definition of the symbol it is defining, and also to keep track of it
        // Duplicate typedefs are allowed in C++ as long as they point to the same underlying type, so we want to emit typedefs of all dependencies for each header we generate
        if ( const CComPtr<IDiaSymbol> TypedefDefinitionSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, type ) )
        {
            HandleTypedefDependency(TypedefName, TypedefDefinitionSymbol);
            CollectDependenciesForType( TypedefDefinitionSymbol, false );
        }
    }
    else if ( SymbolTag == SymTagUDT )
    {
        // Always want the original type if possible, since we can end up here with a CV modified type instead
        CComPtr<IDiaSymbol> SourceUDTType = DiaUtils::RemoveCVModifiersFromType( InTypeSymbol );

        // Whenever we want a declaration or a definition for UDT depends on whenever the type that is enveloping us (for example, a pointer) wants the full definition or not
        AddDependency( TypeDependency::UserDefinedType( InTypeSymbol ), bOnlyWantsPredeclaration );
    }
    else if ( SymbolTag == SymTagFunctionType )
    {
        // We need a full definition for the return type symbol, since it might need to be copied to call the function
        if ( const CComPtr<IDiaSymbol> ReturnTypeSymbol = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, type ) )
        {
            CollectDependenciesForType( ReturnTypeSymbol, false );
        }
        // Object pointer type should always be a pointer when present, so whenever we specify predeclaration here or not does not matter
        if ( const CComPtr<IDiaSymbol> ObjectPointerType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, type ) )
        {
            CollectDependenciesForType( ObjectPointerType, false );
        }

        // Iterate function argument types and handle each one of them
        for ( DiaChildSymbolIterator It( InTypeSymbol, SymTagFunctionArgType ); It; ++It )
        {
            const CComPtr<IDiaSymbol> FunctionArgumentSymbol = *It;
            if ( const CComPtr<IDiaSymbol> FunctionArgumentType = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionArgumentSymbol, type ) )
            {
                CollectDependenciesForType( FunctionArgumentType, false );
            }
        }
    }
    else
    {
        // We should have handled all possible type symbols above. Passing anything else to this function is a mistake on the caller
        std::wcerr << L"Unhandled Symbol Tag: " << SymbolTag << L" for Type Symbol " << GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, name ) << std::endl;
        assert(false);
    }

    // If this type is contained inside of another type, we need to add the dependency to that type too, with definition visibility, since nested types cannot be predeclared
    if ( const CComPtr<IDiaSymbol> ClassParentSymbol = TypeResolver->FindNestedTypeParent( InTypeSymbol ) )
    {
        CollectDependenciesForType( ClassParentSymbol, false );
    }
}

void TypeDependencyCollectorBase::CollectDependenciesForUDTDefinition( const CComPtr<IDiaSymbol>& InUserDefinedType, const bool bInternalMarkAsPredeclaration )
{
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( InUserDefinedType, symTag ) == SymTagUDT );

    // Gather base class dependencies for this type
    for ( DiaChildSymbolIterator It( InUserDefinedType, SymTagBaseClass ); It; ++It )
    {
        const CComPtr<IDiaSymbol> BaseClassSymbol = *It;
        const CComPtr<IDiaSymbol> BaseClassType = GET_SYMBOL_ATTRIBUTE_CHECKED( BaseClassSymbol, type );

        CollectDependenciesForType( BaseClassType, bInternalMarkAsPredeclaration );
    }

    // Gather dependencies for the functions
    for ( DiaChildSymbolIterator It( InUserDefinedType, SymTagFunction ); It; ++It )
    {
        const CComPtr<IDiaSymbol> FunctionSymbol = *It;
        const CComPtr<IDiaSymbol> FunctionSignatureType = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionSymbol, type );

        // Do not collect dependencies for non-virtual functions with no valid location. This introduces unneeded dependencies between headers without any real benefit to the end user.
        const BOOL bIsVirtualFunction = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSymbol, virtual );
        const DWORD FunctionLocationType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSymbol, locationType );
        if ( !bIsVirtualFunction && FunctionLocationType == LocIsNull ) continue;

        CollectDependenciesForType( FunctionSignatureType, bInternalMarkAsPredeclaration );
    }

    // Gather dependencies for the data members
    for ( DiaChildSymbolIterator It( InUserDefinedType, SymTagData ); It; ++It )
    {
        const CComPtr<IDiaSymbol> DataSymbol = *It;
        const CComPtr<IDiaSymbol> VariableDataType = GET_SYMBOL_ATTRIBUTE_CHECKED( DataSymbol, type );

        CollectDependenciesForType( VariableDataType, bInternalMarkAsPredeclaration );
    }
}

void TypeDependencyCollectorBase::CollectDependenciesForTemplateInstantiation( const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments )
{
    std::vector<std::shared_ptr<ITypeDeclaration>> DeclarationsToCheck;
    for ( const TypeTemplateArgument& Argument : TemplateArguments.Arguments )
    {
        if ( Argument.Type == ETemplateArgumentType::TypeDeclaration )
        {
            DeclarationsToCheck.push_back(Argument.TypeConstant);
        }
        if ( Argument.Type == ETemplateArgumentType::TypeMemberReference )
        {
            DeclarationsToCheck.push_back(Argument.TypeMemberReference.OwnerType);
        }
    }

    std::vector<const EnumTypeDeclaration*> TopLevelEnums;
    std::vector<const UDTTypeDeclaration*> TopLevelTypes;

    for ( int32_t i = 0; i < DeclarationsToCheck.size(); i++ )
    {
        const std::shared_ptr<ITypeDeclaration> Decl = DeclarationsToCheck[i];
        if ( Decl->GetId() == ETypeDeclarationId::PointerType )
        {
            const PointerTypeDeclaration* PointerType = static_cast<const PointerTypeDeclaration*>( Decl.get() );
            DeclarationsToCheck.push_back( PointerType->PointeeType );
            if ( PointerType->OwnerType )
            {
                DeclarationsToCheck.push_back( PointerType->OwnerType );
            }
        }
        else if ( Decl->GetId() == ETypeDeclarationId::ArrayType )
        {
            DeclarationsToCheck.push_back( static_cast<const ArrayTypeDeclaration*>( Decl.get() )->ElementType );
        }
        else if ( Decl->GetId() == ETypeDeclarationId::FunctionType )
        {
            const FunctionTypeDeclaration* FunctionType = static_cast<const FunctionTypeDeclaration*>( Decl.get() );
            DeclarationsToCheck.push_back( FunctionType->ReturnType );
            if ( FunctionType->OwnerType )
            {
                DeclarationsToCheck.push_back( FunctionType->OwnerType );
            }
            for (const auto& ArgumentType: FunctionType->Arguments | std::views::values)
            {
                DeclarationsToCheck.push_back(ArgumentType);
            }
        }
        else if ( Decl->GetId() == ETypeDeclarationId::Enum )
        {
            const EnumTypeDeclaration* EnumType = static_cast<const EnumTypeDeclaration*>( Decl.get() );
            if ( EnumType->OuterType == nullptr )
            {
                TopLevelEnums.push_back(EnumType);
            }
            else
            {
                DeclarationsToCheck.push_back(EnumType->OuterType);
            }
        }
        else if ( Decl->GetId() == ETypeDeclarationId::UDT )
        {
            const UDTTypeDeclaration* UDTType = static_cast<const UDTTypeDeclaration*>( Decl.get() );
            if ( UDTType->OuterType == nullptr )
            {
                TopLevelTypes.push_back(UDTType);
            }
            else
            {
                DeclarationsToCheck.push_back(UDTType->OuterType);
            }
        }
    }

    // Collect dependencies on the top level enums first, since they cannot be templated
    for ( const EnumTypeDeclaration* EnumDeclaration : TopLevelEnums )
    {
        SymbolNameInfo SymbolNameInfo;
        SymbolNameInfo.SymbolScope = EnumDeclaration->OuterScope;
        SymbolNameInfo.LocalName = EnumDeclaration->EnumName;

        // Resolve the underlying enum type from the the symbol storage. We should ALWAYS have data for all types used as template instantiation arguments.
        // At least, cases where the data is not available should be the vast minority and need to be carefully investigated.
        const CComPtr<IDiaSymbol> EnumerationSymbol = TypeResolver->ResolveEnumByFullName( SymbolNameInfo.ToString( SymbolNameInfo::IncludeNamespace ) );
        assert( EnumerationSymbol && L"Failed to resolve enumeration type used as a template instantiation argument" );

        // We only want the pre-declaration for the template instantiation enum parameters, full definition is never necessary under any conditions
        if ( EnumerationSymbol )
        {
            AddDependency( TypeDependency::Enum( EnumerationSymbol ), true );
        }
    }

    // Some templates need full typename arguments, and others only need pre-declarations
    const bool bTemplateNeedsFullType = TypeResolver->DoTemplateArgumentsNeedFullDefinition( TemplateName );

    // Collect dependencies on the top level UDTs now. These can very much be templated and as such we need to use a slower lookup in case the template data is used
    for ( const UDTTypeDeclaration* TypeDeclaration : TopLevelTypes )
    {
        SymbolNameInfo SymbolNameInfo;
        SymbolNameInfo.SymbolScope = TypeDeclaration->OuterScope;
        SymbolNameInfo.LocalName = TypeDeclaration->ClassName;

        // If there are no template arguments, we can do fast resolution based only on the full symbol name
        if ( TypeDeclaration->TemplateArguments.Arguments.empty() )
        {
            const CComPtr<IDiaSymbol> UDTSymbol = TypeResolver->ResolveUDTByFullName( SymbolNameInfo.ToString( SymbolNameInfo::IncludeNamespace ) );

            // We only need a pre-declaration for this type since it has no template arguments
            if ( UDTSymbol )
            {
                AddDependency( TypeDependency::UserDefinedType( UDTSymbol ), !bTemplateNeedsFullType );
            }
            else
            {
                std::wcerr << L"Failed to resolve UDT type used as a template instantiation argument: " << SymbolNameInfo.ToString() << L". This will result in a compilation error unless manual definition is provided." << std::endl;
            }
        }
        // If we have template arguments, we need to either resolve the template instantiation symbol (which is extremely slow for templates with huge amount of instantiations)
        // Or attempt to resolve the dependency without having to resolve the name into the symbol
        else
        {
            // Always collect dependencies for template instantiation regardless of whenever we do fast or slow template processing
            CollectDependenciesForTemplateInstantiation( SymbolNameInfo, TypeDeclaration->TemplateArguments );

            // Check if this is a symbol that we consider special or external. If it is, lookup the external file
            if ( HandleTemplateInstantiationDependency( SymbolNameInfo, TypeDeclaration->TemplateArguments ) )
            {
                continue;
            }

            // Use slow path using the iteration across all template instantiations in the database in case this is our own type.
            if ( const CComPtr<IDiaSymbol> InstantiationSymbol = TypeResolver->ResolveTemplateInstantiation( SymbolNameInfo, TypeDeclaration->TemplateArguments ) )
            {
                AddDependency( TypeDependency::UserDefinedType( InstantiationSymbol ), !bTemplateNeedsFullType );
            }
            else
            {
                FormattedTextWriter TemplateArgumentsText;
                TypeDeclaration->TemplateArguments.Print(TemplateArgumentsText, TypeFormattingRules());

                // This a pretty serious error that will result in a compilation error, but it is normal for some template types in some cases (e.g. non-type arguments only templates) to be absent
                std::wcerr << L"Failed to resolve template instantiation used as a template argument for non-external type " << SymbolNameInfo.ToString() << L". Template Arguments: " << TemplateArgumentsText.ToString() << std::endl;
            }
        }
    }
}

void TypeDependencyCollectorBase::AddDependency(const TypeDependency& TypeDependency, bool bIsPredeclaration)
{
    // Nested types cannot be pre-declared.
    if ( TypeResolver->FindNestedTypeParent( TypeDependency.GetSymbol() ) )
    {
        bIsPredeclaration = false;
    }
    AddDependencyInternal( TypeDependency, bIsPredeclaration );
}

TypeDependencyCollector::TypeDependencyCollector( ITypeResolutionProvider* InTypeResolver, const bool InRecurseIntoUserDefinedTypes) : TypeDependencyCollectorBase( InTypeResolver ), bRecurseIntoUserDefinedTypes( InRecurseIntoUserDefinedTypes )
{
}

std::unordered_set<TypeDependency> TypeDependencyCollector::GetAllDependencies() const
{
    std::unordered_set<TypeDependency> AllDependencies;
    AllDependencies.insert(DeclarationDependencies.begin(), DeclarationDependencies.end());
    AllDependencies.insert(DefinitionDependencies.begin(), DefinitionDependencies.end());
    return AllDependencies;
}

void TypeDependencyCollector::HandleTypedefDependency(const std::wstring& TypedefName, const CComPtr<IDiaSymbol>& TypedefType)
{
    TypedefDependencies.insert({TypedefName, TypedefType});
}

void TypeDependencyCollector::AddDependencyInternal(const TypeDependency& TypeDependency, bool bIsPredeclaration)
{
    bool bIsNewDependency = false;
    if ( bIsPredeclaration )
    {
        // Only add pre-declaration dependency if we do not already have a definition dependency for the same type
        if ( !DefinitionDependencies.contains(TypeDependency) )
        {
            bIsNewDependency = DeclarationDependencies.insert(TypeDependency).second;
        }
    }
    else
    {
        // Erase declaration dependency if we are adding a definition for the same type
        if ( DefinitionDependencies.insert(TypeDependency).second )
        {
            DeclarationDependencies.erase(TypeDependency);
            bIsNewDependency = true;
        }
    }

    // If we want to recurse into the user defined types referenced, collect all of the dependencies needed for the definition of the type
    if ( bIsNewDependency && bRecurseIntoUserDefinedTypes && TypeDependency.IsUserDefinedType() )
    {
        const DWORD UniqueSymbolId = GET_SYMBOL_ATTRIBUTE_CHECKED( TypeDependency.GetSymbol(), symIndexId );

        if ( !UserDefinedTypesHandled.contains( UniqueSymbolId ) )
        {
            UserDefinedTypesHandled.insert( UniqueSymbolId );
            CollectDependenciesForUDTDefinition( TypeDependency.GetSymbol(), false );
        }
    }
}
