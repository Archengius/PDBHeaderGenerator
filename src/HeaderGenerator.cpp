#include "HeaderGenerator.h"
#include "HeaderGeneratorConfig.h"
#include "COFFImportLibrary.h"
#include "CodeGeneration.h"
#include "AST/CppDeclarationTree.h"
#include "GeneratedHeaderFile.h"
#include "Utils/StringUtils.h"
#include "Utils/MemoryGrowableBuffer.h"

#include <iostream>
#include <ranges>
#include <filesystem>

CompilationUnit::CompilationUnit( HeaderGenerator* InOwnerGenerator, const CComPtr<IDiaSymbol>& InCompilationUnitSymbol, const std::wstring& InCompilationUnitName, const bool InIsExternalCompilationUnit ) :
    OwnerGenerator( InOwnerGenerator ), CompilationUnitSymbol( InCompilationUnitSymbol ), CompilationUnitName( InCompilationUnitName ), bIsExternalCompilationUnit( InIsExternalCompilationUnit )
{
}

int32_t CompilationUnit::EstimateActionCount() const
{
    return 2; // one for ProcessCompilandFunctionsAndData, and one for NeedsHeaderForCompilationUnit
}

void CompilationUnit::ProcessActions_Pass1( int32_t& CurrentActionNumber )
{
    if ( !bIsGlobalScopeUnit )
    {
        ProcessCompilandDetails();
        ProcessCompilandEnvironment();
        ProcessCompilandFunctionsAndData( CurrentActionNumber );

        // We only support X64 platform with C++ headers right now, results for other languages will most likely not make sense because the languages have very different semantics.
        if ( !bIsExternalCompilationUnit && ( CompilandPlatform != CV_CFL_X64 || CompilandSourceLanguage != CV_CFL_CXX ) )
        {
            std::wcerr << L"Unusual Compiland Platform/Language combo found for Object File " << CompilationUnitName << ". Generated headers might not be reasonable. Platform: " << CompilandPlatform << ", Language: " << CompilandSourceLanguage << std::endl;
        }
    }
    CollectAndRegisterDependencies( CurrentActionNumber );
}

void CompilationUnit::ProcessActions_Pass2( int32_t& CurrentActionNumber )
{
    if ( NeedsHeaderForCompilationUnit() )
    {
        GeneratedHeaderFile* HeaderFile = OwnerGenerator->FindOrCreateHeaderFile( GetHeaderFilename() );
        PushGlobalMembersToHeaderFile( HeaderFile );
    }
}

void CompilationUnit::ProcessCompilandEnvironment()
{
    for ( DiaChildSymbolIterator It( CompilationUnitSymbol, SymTagCompilandEnv ); It; ++It )
    {
        CComPtr<IDiaSymbol> Symbol = *It;
        const std::wstring SymbolName = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(Symbol, name);
        const CComVariant SymbolValue = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(Symbol, value);

        if ( !SymbolName.empty() && SymbolValue.vt == VT_BSTR )
        {
            CompilandEnvironment.insert({SymbolName, SymbolValue.bstrVal});
        }
    }
}

void CompilationUnit::ProcessCompilandDetails()
{
    for ( DiaChildSymbolIterator It( CompilationUnitSymbol, SymTagCompilandDetails ); It; ++It )
    {
        CComPtr<IDiaSymbol> Symbol = *It;
        CompilandPlatform = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(Symbol, platform);
        CompilandSourceLanguage = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(Symbol, language);
    }
}

void CompilationUnit::ProcessCompilandFunctionsAndData( int32_t& CurrentActionNumber )
{
    const std::wstring PrefixLogString = StringPrintf(L"[%d/%d] %s: ", CurrentActionNumber++, OwnerGenerator->GetTotalActionCount(), CompilationUnitName.c_str());
    std::wcout << PrefixLogString << L"Processing Functions & Data" << (bIsExternalCompilationUnit ? L" (External Unit)" : L"") << std::endl;

     for ( DiaChildSymbolIterator It( CompilationUnitSymbol, SymTagFunction ); It; ++It )
    {
        CComPtr<IDiaSymbol> Symbol = *It;

        // Skip thunk functions. These functions will have no valid type and instead just represent a label that's target for SEH, or something like that
        if ( HeaderGenerator::IsFunctionGeneratedThunk( Symbol ) ) continue;

        // Skip internal functions
        const SymbolNameInfo FunctionName = SymbolNameInfo::FromSymbol( Symbol );
        if ( CodeGeneration::IsInternalSymbolName( FunctionName ) ) continue;
        // Skip external functions
        if ( OwnerGenerator->ShouldMarkSymbolAsExternal( FunctionName, EExternalSymbolType::Function ) ) continue;

        AllFunctionImplementations.push_back( Symbol );
        OwnerGenerator->PushDefinedSymbol( FunctionName.OriginalFullName );

        // There is a class parent for this function, which means it is a member function for some type
        // Since a type can be defined in multiple different object files, let the type definition decide which file the function should be placed into
        if (const CComPtr<IDiaSymbol> ClassParent = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(Symbol, classParent ) )
        {
            // Skip this function if this is an override of a parent function that actually shares the code with it
            if ( GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( Symbol, virtual ) )
            {
                const CComPtr<IDiaSymbol> FunctionSignatureType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( Symbol, type );
                const CComPtr<IDiaSymbol> ParentClassFunction = DiaUtils::FindParentVirtualFunction( ClassParent, FunctionName.LocalName, FunctionSignatureType );

                if ( ParentClassFunction && DiaUtils::IsFunctionImplementationTheSame( Symbol, ParentClassFunction ) )
                {
                    continue;
                }
            }

            // Do not create user defined types for external objects, register the type as external instead
            if ( !IsExternalCompilationUnit() )
            {
                // We can end up with an external type here as a parent for the function. This is not a logical error, but rather a function from the template type defined in external type being inlined into this object file
                // To correctly handle this, we need to completely ignore that function definition
                if ( !OwnerGenerator->IsExternalUserDefinedType( ClassParent ) )
                {
                    // Check if this class has been marked as external by the user explicitly, and then mark it as external type if so
                    const CComPtr<IDiaSymbol> TopLevelClass = OwnerGenerator->FindTopLevelType( DiaUtils::RemoveCVModifiersFromType( ClassParent ) );
                    if ( !OwnerGenerator->ShouldMarkSymbolAsExternal( SymbolNameInfo::FromSymbol( TopLevelClass ), EExternalSymbolType::Type ) )
                    {
                        OwnerGenerator->FindOrAddUserDefinedType( ClassParent )->AddDefinedFunction( Symbol, this );
                    }
                    else
                    {
                        OwnerGenerator->RegisterExternalUserDefinedType( ClassParent );
                    }
                }
            }
            else
            {
                OwnerGenerator->RegisterExternalUserDefinedType( ClassParent );
            }
        }
        else
        {
            if ( IsExternalCompilationUnit() )
            {
                OwnerGenerator->RegisterExternalSymbolByRVA(Symbol);
            }
            GlobalFunctions.push_back( Symbol );
        }
    }

    for ( DiaChildSymbolIterator It( CompilationUnitSymbol, SymTagData ); It; ++It )
    {
        CComPtr<IDiaSymbol> Symbol = *It;

        // Skip compiler generated data symbols, like virtual function tables or initializers
        if ( GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( Symbol, compilerGenerated ) ) continue;

        // Skip data we can infer to be compiler generated as well
        const SymbolNameInfo DataName = SymbolNameInfo::FromSymbol( Symbol );
        if ( CodeGeneration::IsInternalSymbolName( DataName ) ) continue;
        // Skip external data
        if ( OwnerGenerator->ShouldMarkSymbolAsExternal( DataName, EExternalSymbolType::Data ) ) continue;

        AllDataMemberDefinitions.push_back( Symbol );
        OwnerGenerator->PushDefinedSymbol( DataName.OriginalFullName );

        // There is a class parent for this variable, which means it is a member function for some type
        // Since a type can be defined in multiple different object files, let the type definition decide which file the function should be placed into
        if ( const CComPtr<IDiaSymbol> ClassParent = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(Symbol, classParent) )
        {
            // Do not create user defined types for external objects, register the type as external instead
            if ( !IsExternalCompilationUnit() )
            {
                // We can end up with an external type here as a parent for the data. This is not a logical error, but rather a function from the template type defined in external type being inlined into this object file
                // To correctly handle this, we need to completely ignore that data definition
                if ( !OwnerGenerator->IsExternalUserDefinedType( ClassParent ) )
                {
                    // Check if this class has been marked as external by the user explicitly, and then mark it as external type if so
                    const CComPtr<IDiaSymbol> TopLevelClass = OwnerGenerator->FindTopLevelType( DiaUtils::RemoveCVModifiersFromType( ClassParent ) );
                    if ( !OwnerGenerator->ShouldMarkSymbolAsExternal( SymbolNameInfo::FromSymbol( TopLevelClass ), EExternalSymbolType::Type ) )
                    {
                        OwnerGenerator->FindOrAddUserDefinedType( ClassParent )->AddDefinedVariable( Symbol, this );
                    }
                    else
                    {
                        OwnerGenerator->RegisterExternalUserDefinedType( ClassParent );
                    }
                }
            }
            else
            {
                OwnerGenerator->RegisterExternalUserDefinedType( ClassParent );
            }
        }
        else
        {
            if ( IsExternalCompilationUnit() )
            {
                OwnerGenerator->RegisterExternalSymbolByRVA(Symbol);
            }
            GlobalVariables.push_back( Symbol );
        }
    }
}

void CompilationUnit::CollectAndRegisterDependencies( int32_t& CurrentActionNumber )
{
    const std::wstring PrefixLogString = StringPrintf(L"[%d/%d] %s: ", CurrentActionNumber++, OwnerGenerator->GetTotalActionCount(), CompilationUnitName.c_str());
    std::wcout << PrefixLogString << L"Collecting Referenced Types" << ( bIsExternalCompilationUnit ? L" (External Unit)" : L"" ) << std::endl;

    CompilationUnitReferenceCollector TypeDendencyCollector( this );

    for ( const CComPtr<IDiaSymbol>& GlobalFunction : AllFunctionImplementations )
    {
        const CComPtr<IDiaSymbol> FunctionType = GET_SYMBOL_ATTRIBUTE_CHECKED( GlobalFunction, type );
        TypeDendencyCollector.CollectDependenciesForType( FunctionType );
    }
    for ( const CComPtr<IDiaSymbol>& GlobalVariable : AllDataMemberDefinitions )
    {
        const CComPtr<IDiaSymbol> DataType = GET_SYMBOL_ATTRIBUTE_CHECKED( GlobalVariable, type );
        TypeDendencyCollector.CollectDependenciesForType( DataType );
    }
}

CompilationUnitReferenceCollector::CompilationUnitReferenceCollector(CompilationUnit* InCompilationUnit) : TypeDependencyCollectorBase( InCompilationUnit->GetHeaderGenerator() ), mCompilationUnit( InCompilationUnit )
{
}

void CompilationUnitReferenceCollector::AddDependencyInternal(const TypeDependency& TypeDependency, bool bIsPredeclaration)
{
    const DWORD SymbolIndex = GET_SYMBOL_ATTRIBUTE_CHECKED( TypeDependency.GetSymbol(), symIndexId );
    if ( VisitedTypes.contains( SymbolIndex ) ) return;
    VisitedTypes.insert( SymbolIndex );

    if ( TypeDependency.IsUserDefinedType() )
    {
        // If class is a template instantiation, we need to collect dependencies to it's arguments, since otherwise we can completely miss some user generated types
        const SymbolNameInfo DependencyName = SymbolNameInfo::FromSymbol( TypeDependency.GetSymbol() );
        if ( DependencyName.bIsTemplateInstantiation )
        {
            TypeTemplateArgumentContainer TemplateArguments;
            const bool bParsedArguments = TypeTemplateArgumentContainer::ParseTemplateArguments( DependencyName.TemplateArguments, TemplateArguments );
            assert( bParsedArguments && L"Failed to parse template arguments for user-defined template instantiation" );

            CollectDependenciesForTemplateInstantiation( DependencyName, TemplateArguments );
        }

        if ( mCompilationUnit->IsExternalCompilationUnit() )
        {
            // Only recurse into the user defined type if we have not already registered it
            if ( mCompilationUnit->GetHeaderGenerator()->RegisterExternalUserDefinedType( TypeDependency.GetSymbol() ) )
            {
                CollectDependenciesForUDTDefinition( TypeDependency.GetSymbol() );
            }
        }
        else if ( !mCompilationUnit->GetHeaderGenerator()->IsExternalUserDefinedType( TypeDependency.GetSymbol() ) )
        {
            // Check if this class has been marked as external by the user explicitly, and then mark it as external type if so
            const CComPtr<IDiaSymbol> UnmodifiedDependencyClass = DiaUtils::RemoveCVModifiersFromType( TypeDependency.GetSymbol() );
            const CComPtr<IDiaSymbol> TopLevelClass = TypeResolver->FindTopLevelType( UnmodifiedDependencyClass );

            const SymbolNameInfo TopLevelClassName = SymbolNameInfo::FromSymbol( TopLevelClass );

            if ( !mCompilationUnit->GetHeaderGenerator()->ShouldMarkSymbolAsExternal( TopLevelClassName, EExternalSymbolType::Type ) )
            {
                UserDefinedTypeInfo* UserDefinedType = mCompilationUnit->GetHeaderGenerator()->FindOrAddUserDefinedType( TypeDependency.GetSymbol() );

                if ( UserDefinedType->AddCompilationUnitReference( mCompilationUnit ) )
                {
                    CollectDependenciesForUDTDefinition( TypeDependency.GetSymbol() );
                }
            }
            else
            {
                mCompilationUnit->GetHeaderGenerator()->RegisterExternalUserDefinedType( TypeDependency.GetSymbol() );
            }
        }
    }
    else if ( TypeDependency.IsEnum() )
    {
        if ( mCompilationUnit->IsExternalCompilationUnit() )
        {
            mCompilationUnit->GetHeaderGenerator()->RegisterExternalUserDefinedType( TypeDependency.GetSymbol() );
        }
        else if ( !mCompilationUnit->GetHeaderGenerator()->IsExternalUserDefinedType( TypeDependency.GetSymbol() ) )
        {
            // Check if this class has been marked as external by the user explicitly, and then mark it as external type if so
            const CComPtr<IDiaSymbol> TopLevelClass = mCompilationUnit->GetHeaderGenerator()->FindTopLevelType( TypeDependency.GetSymbol() );
            if ( !mCompilationUnit->GetHeaderGenerator()->ShouldMarkSymbolAsExternal( SymbolNameInfo::FromSymbol( TopLevelClass ), EExternalSymbolType::Type ) )
            {
                EnumerationInfo* EnumerationInfo = mCompilationUnit->GetHeaderGenerator()->FindOrAddEnum( TypeDependency.GetSymbol() );
                EnumerationInfo->AddCompilationUnitReference( mCompilationUnit );
            }
            else
            {
                mCompilationUnit->GetHeaderGenerator()->RegisterExternalUserDefinedType( TypeDependency.GetSymbol() );
            }
        }
    }
}

bool CompilationUnitReferenceCollector::HandleTemplateInstantiationDependency(const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments)
{
    // Do not recurse into any external or special template instantiations, we do not need to look their UDTs up as it will take huge amount of time
    // We can register them as external templates though to save time later
    if ( mCompilationUnit->IsExternalCompilationUnit() || mCompilationUnit->GetHeaderGenerator()->ShouldMarkSymbolAsExternal( TemplateName, EExternalSymbolType::Type ) || CodeGeneration::IsInternalSymbolName( TemplateName ) )
    {
        mCompilationUnit->GetHeaderGenerator()->RegisterExternalTemplate( TemplateName );
        return true;
    }
    return false;
}

void CompilationUnit::PushGlobalMembersToHeaderFile( GeneratedHeaderFile* HeaderFile ) const
{
    for ( const CComPtr<IDiaSymbol>& GlobalFunction : GlobalFunctions )
    {
        const SymbolNameInfo SymbolName = SymbolNameInfo::FromSymbol( GlobalFunction );

        // Push duplicate symbol definitions into their own header file. Since we do not account for template arguments, this will push all global variable template instantiations into a single file conveniently
        if ( OwnerGenerator->IsDuplicateSymbolDefinition( SymbolName.OriginalFullName ) || SymbolName.bIsTemplateInstantiation )
        {
            std::wstring DuplicateDefinitionHeaderName = SymbolName.LocalName;

            // If we have a conflict with a manually overriden header filename, append Generated suffix to the header name
            if ( OwnerGenerator->IsHeaderFilenameOccupiedByManualHeader( DuplicateDefinitionHeaderName ) )
            {
                DuplicateDefinitionHeaderName.append(L"_Generated");
            }
            GeneratedHeaderFile* SymbolDestinationHeaderFile = OwnerGenerator->FindOrCreateHeaderFile( DuplicateDefinitionHeaderName );
            SymbolDestinationHeaderFile->AddGlobalFunction( GlobalFunction );
            continue;
        }

        // Exclude symbols that are not public from being generated since they are not addressable from the outside code and are an implementation detail
        const DWORD SymbolLocationType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( GlobalFunction, locationType );
        if ( SymbolLocationType == LocIsStatic )
        {
            const DWORD SymbolRelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_CHECKED( GlobalFunction, relativeVirtualAddress );
            if ( OwnerGenerator->FindPublicSymbolByRVA( SymbolRelativeVirtualAddress ) == nullptr )
            {
                continue;
            }
        }
        HeaderFile->AddGlobalFunction( GlobalFunction );
    }
    for ( const CComPtr<IDiaSymbol>& GlobalVariable : GlobalVariables )
    {
        const SymbolNameInfo SymbolName = SymbolNameInfo::FromSymbol( GlobalVariable );

        // Push duplicate symbol definitions into their own header file. Since we do not account for template arguments, this will push all global function template instantiations into a single file conveniently
        if ( OwnerGenerator->IsDuplicateSymbolDefinition( SymbolName.OriginalFullName ) || SymbolName.bIsTemplateInstantiation )
        {
            std::wstring DuplicateDefinitionHeaderName = SymbolName.LocalName;

            // If we have a conflict with a manually overriden header filename, append Generated suffix to the header name
            if ( OwnerGenerator->IsHeaderFilenameOccupiedByManualHeader( DuplicateDefinitionHeaderName ) )
            {
                DuplicateDefinitionHeaderName.append(L"_Generated");
            }
            GeneratedHeaderFile* SymbolDestinationHeaderFile = OwnerGenerator->FindOrCreateHeaderFile( DuplicateDefinitionHeaderName );
            SymbolDestinationHeaderFile->AddGlobalVariable( GlobalVariable );
            continue;
        }

        // Exclude symbols that are not public from being generated since they are not addressable from the outside code and are an implementation detail
        const DWORD SymbolLocationType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( GlobalVariable, locationType );
        if ( SymbolLocationType == LocIsStatic )
        {
            const DWORD SymbolRelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_CHECKED( GlobalVariable, relativeVirtualAddress );
            if ( OwnerGenerator->FindPublicSymbolByRVA( SymbolRelativeVirtualAddress ) == nullptr )
            {
                continue;
            }
        }
        HeaderFile->AddGlobalVariable( GlobalVariable );
    }
}

bool CompilationUnit::NeedsHeaderForCompilationUnit() const
{
    // We need a header for this object file if it has global functions or variables defined that are not contained by any UDT
    return !bIsExternalCompilationUnit && (!GlobalFunctions.empty() || !GlobalVariables.empty());
}

std::wstring CompilationUnit::GetHeaderFilename() const
{
    std::wstring ResultCompilationUnitHeaderName;

    // Prefer source .cpp filename instead of the .obj filename
    if (const auto SourceFilenameIterator = CompilandEnvironment.find( L"src" ); SourceFilenameIterator != CompilandEnvironment.end() )
    {
        ResultCompilationUnitHeaderName = std::filesystem::path( SourceFilenameIterator->second ).filename().replace_extension().wstring();
    }
    else
    {
        // Fallback to obj filename in case we do not have a valid source filename
        ResultCompilationUnitHeaderName = CompilationUnitName;
    }

    // If we have a conflict with a manually overriden header filename, append Generated suffix to the header name
    if ( OwnerGenerator->IsHeaderFilenameOccupiedByManualHeader( ResultCompilationUnitHeaderName ) )
    {
        ResultCompilationUnitHeaderName.append(L"_Generated");
    }

    return ResultCompilationUnitHeaderName;
}

UserDefinedTypeInfo::UserDefinedTypeInfo( HeaderGenerator* InGenerator, const CComPtr<IDiaSymbol>& InUDTSymbol) : OwnerGenerator( InGenerator ), UserDefinedTypeSymbol( DiaUtils::RemoveCVModifiersFromType( InUDTSymbol ) )
{
    // We should never create class definitions for non-UDT symbols
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( InUDTSymbol, symTag ) == SymTagUDT );
    SymbolName = SymbolNameInfo::FromSymbol( UserDefinedTypeSymbol );
}

void UserDefinedTypeInfo::AddDefinedFunction( const CComPtr<IDiaSymbol>& FunctionSymbol, const CompilationUnit* CompilationUnit )
{
    if ( !CompilationUnitDefinitions.contains( CompilationUnit ) )
    {
        CompilationUnitDefinitions.insert({ CompilationUnit, std::vector<CComPtr<IDiaSymbol>>() });
    }
    CompilationUnitDefinitions.find(CompilationUnit)->second.push_back( FunctionSymbol );
}

void UserDefinedTypeInfo::AddDefinedVariable( const CComPtr<IDiaSymbol>& FieldSymbol, const CompilationUnit* CompilationUnit )
{
    if ( !CompilationUnitDefinitions.contains( CompilationUnit ) )
    {
        CompilationUnitDefinitions.insert({ CompilationUnit, std::vector<CComPtr<IDiaSymbol>>() });
    }
    CompilationUnitDefinitions.find(CompilationUnit)->second.push_back( FieldSymbol );
}

void UserDefinedTypeInfo::AddNestedUserDefinedType( UserDefinedTypeInfo* InNestedUDT )
{
    NestedTypes.push_back( InNestedUDT );
}

void UserDefinedTypeInfo::AddNestedEnumeration(EnumerationInfo* InNestedEnumeration)
{
    NestedEnums.push_back( InNestedEnumeration );
}

bool UserDefinedTypeInfo::AddCompilationUnitReference(const CompilationUnit* FromCompilationUnit)
{
    // We should recurse into the symbol if this unit is one of the units that provides definitions for this type, or if we are the first unit to reference the symbol
    CompilationUnitReferences.insert( FromCompilationUnit );
    return CompilationUnitDefinitions.contains( FromCompilationUnit ) || CompilationUnitReferences.size() == 1;
}

void UserDefinedTypeInfo::ProcessActions()
{
    // Remove definitions of symbols that are marked as duplicate, and remove definition compilation unit references if they only contained duplicate symbols
    std::erase_if( CompilationUnitDefinitions, [&]( std::pair<const CompilationUnit* const, std::vector<CComPtr<IDiaSymbol>>>& Pair )
    {
        std::erase_if( Pair.second, [&]( const CComPtr<IDiaSymbol>& Symbol )
        {
            const std::wstring SymbolName = GET_SYMBOL_ATTRIBUTE_CHECKED( Symbol, name );
            return OwnerGenerator->IsDuplicateSymbolDefinition( SymbolName );
        } );
        return Pair.second.empty();
    } );

    // Do not generate anonymous types. They will be pulled in on demand into their declarations.
    if ( SymbolName.bIsAnonymousSymbol || SymbolName.bIsLambdaSymbol )
    {
        return;
    }

    // If we have a symbol that is our class parent, we need to push ourselves to all class parent to generate us, instead of directly pushing ourselves to some header
    if ( const CComPtr<IDiaSymbol> ClassParentSymbol = OwnerGenerator->FindNestedTypeParent( UserDefinedTypeSymbol ) )
    {
        // Since header generator iterates by index and goes from 0 to length, it is safe to potentially resize array vector here
        UserDefinedTypeInfo* ParentTypeInfo = OwnerGenerator->FindOrAddUserDefinedType( ClassParentSymbol );
        assert( ParentTypeInfo != nullptr && L"Nested types inside of anonymous types are not supported" );
        if ( ParentTypeInfo )
        {
            ParentTypeInfo->AddNestedUserDefinedType( this );
        }
        return;
    }

    // Skip generation of types residing in the manually provided overriden headers
    if ( OwnerGenerator->FindOverridenManualHeaderForType( SymbolName ) != nullptr )
    {
        return;
    }
    // Associate the type with the generated header file
    OwnerGenerator->FindOrCreateHeaderFile( GetHeaderNameForType() )->AddTopLevelType( this );
}

EnumerationInfo::EnumerationInfo(HeaderGenerator* InGenerator, const CComPtr<IDiaSymbol>& InEnumSymbol) : OwnerGenerator( InGenerator ), EnumerationSymbol( InEnumSymbol )
{
}

void EnumerationInfo::ProcessActions()
{
    // If this is a nested enumeration, add it to it's parent type
    if ( const CComPtr<IDiaSymbol> ClassParentSymbol = OwnerGenerator->FindNestedTypeParent( EnumerationSymbol ) )
    {
        // We should never have to "create" a new type here, because if this is a nested enum the reference to it's parent would have already been collected by the dependency collector
        // That means all the references to this enum will also be recorded as references to it's parent type
        UserDefinedTypeInfo* ParentUserDefinedType = OwnerGenerator->FindOrAddUserDefinedType( ClassParentSymbol );
        assert( ParentUserDefinedType && L"Enumerations defined inside of anonymous types are not supported" );
        if ( ParentUserDefinedType )
        {
            ParentUserDefinedType->AddNestedEnumeration( this );
        }
    }
    else
    {
        // Otherwise, it's a top level enumeration, so associate it with a header
        GeneratedHeaderFile* HeaderFile = OwnerGenerator->FindOrCreateHeaderFile( DetermineHeaderFilename() );
        HeaderFile->AddTopLevelEnumeration( this );
    }
}

void EnumerationInfo::AddCompilationUnitReference(const CompilationUnit* FromCompilationUnit)
{
    CompilationUnitReferences.insert(FromCompilationUnit);
}

std::wstring EnumerationInfo::DetermineHeaderFilename() const
{
    std::wstring ResultHeaderFilename;
    if ( CompilationUnitReferences.size() == 1 )
    {
        ResultHeaderFilename = (*CompilationUnitReferences.begin())->GetHeaderFilename();
    }
    else
    {
        const SymbolNameInfo SymbolInfo = SymbolNameInfo::FromSymbol( EnumerationSymbol );
        ResultHeaderFilename = SymbolInfo.LocalName;
    }

    // If we have a conflict with a manually overriden header filename, append Generated suffix to the header name
    if ( OwnerGenerator->IsHeaderFilenameOccupiedByManualHeader( ResultHeaderFilename ) )
    {
        ResultHeaderFilename.append(L"_Generated");
    }
    return ResultHeaderFilename;
}

static std::wstring RemovePostfixFromTypeName( const std::wstring& InTypeName )
{
    if ( InTypeName.ends_with(L"_t") )
    {
        return InTypeName.substr(0, InTypeName.size() - 2);
    }
    return InTypeName;
}

static bool AppendTypeDeclarationToFilename( std::wstring& OutTypeDeclaration, const std::shared_ptr<ITypeDeclaration>& TypeDeclaration )
{
    // We are only interested in types affecting the includes, e.g. UDTs and enums. That means, fundamental types are not accounted for, and neither are integer constants
    // TODO: Do we want to also handle function types here? Since they have type dependencies, but I feel like the resulting name would be way too verbose
    if ( TypeDeclaration->GetId() == ETypeDeclarationId::Enum )
    {
        OutTypeDeclaration = RemovePostfixFromTypeName( static_cast<const EnumTypeDeclaration*>( TypeDeclaration.get() )->EnumName );
        return true;
    }
    if ( TypeDeclaration->GetId() == ETypeDeclarationId::UDT )
    {
        const UDTTypeDeclaration* UDTDeclaration = static_cast<const UDTTypeDeclaration*>( TypeDeclaration.get() );
        std::wostringstream ResultName;
        ResultName << RemovePostfixFromTypeName( UDTDeclaration->ClassName );

        // Append template instantiation arguments to the name
        for ( const TypeTemplateArgument& TemplateArgument : UDTDeclaration->TemplateArguments.Arguments )
        {
            std::wstring TemplateName;
            if ( TemplateArgument.Type == ETemplateArgumentType::TypeDeclaration && AppendTypeDeclarationToFilename( TemplateName, TemplateArgument.TypeConstant ) )
            {
                ResultName << L"_" << TemplateName;
            }
        }
        OutTypeDeclaration = ResultName.str();
        return true;
    }
    // Array and pointer types are only relevant if their underlying type is
    if ( TypeDeclaration->GetId() == ETypeDeclarationId::PointerType )
    {
        return AppendTypeDeclarationToFilename( OutTypeDeclaration, static_cast<const PointerTypeDeclaration*>( TypeDeclaration.get() )->PointeeType );
    }
    if ( TypeDeclaration->GetId() == ETypeDeclarationId::ArrayType )
    {
        return AppendTypeDeclarationToFilename( OutTypeDeclaration, static_cast<const ArrayTypeDeclaration*>( TypeDeclaration.get() )->ElementType );
    }
    return false;
}

std::wstring UserDefinedTypeInfo::GetHeaderNameForType() const
{
    // We are not interested in the namespace of the class, since we just put all headers into the root folder. Later we can sort them out by the namespace potentially
    std::wostringstream ResultFilename;
    ResultFilename << RemovePostfixFromTypeName( SymbolName.LocalName );

    // If this is a template instantiation, we need to mangle the name of the header to make it unique per specialization but also not include any invalid characters
    if ( SymbolName.bIsTemplateInstantiation )
    {
        std::vector<TypeTemplateArgument> TemplateArguments;
        const bool bArgumentsParsed = TypeTemplateArgument::ParseTemplateArguments( SymbolName.TemplateArguments, TemplateArguments, false );
        assert( bArgumentsParsed && L"Failed to parse template instantiation arguments for a type" );

        for ( const TypeTemplateArgument& Argument : TemplateArguments )
        {
            std::wstring ArgumentName;
            if ( Argument.Type == ETemplateArgumentType::TypeDeclaration && AppendTypeDeclarationToFilename( ArgumentName, Argument.TypeConstant ) )
            {
                ResultFilename << L"_" << ArgumentName;
            }
        }
    }

    // If we have a conflict with a manually overriden header filename, append Generated suffix to the header name
    if ( OwnerGenerator->IsHeaderFilenameOccupiedByManualHeader( ResultFilename.str() ) )
    {
        ResultFilename << L"_Generated";
    }
    return ResultFilename.str();
}

HeaderGenerator::HeaderGenerator(const std::wstring& InDllName, const std::filesystem::path& InOutputDirectory) : OutputDirectoryPath( InOutputDirectory), DllName(InDllName)
{
}

void HeaderGenerator::LoadDataFromConfig(const HeaderGeneratorConfig& Config)
{
    for ( const ExternalHeaderDefinition& ExternalHeader : Config.ExternalHeaders )
    {
        const std::wstring ExternalHeaderName = StringUtf8ToWide(ExternalHeader.IncludeName);
        for ( const std::string& ExternalTypeName : ExternalHeader.ContainedTypes )
        {
            ExternalTypeToHeaderLookup.insert({ StringUtf8ToWide(ExternalTypeName), ExternalHeaderName });
        }
        for ( const std::string& ExternalNamespace : ExternalHeader.ContainedNamespaces )
        {
            ExternalNamespaceToHeaderFallback.push_back({ StringUtf8ToWide(ExternalNamespace), ExternalHeaderName });
        }
    }
    for ( const std::string& ExternalFunctionName : Config.ExternalGlobalFunctions )
    {
        ExternalFunctions.insert(StringUtf8ToWide(ExternalFunctionName));
    }
    for ( const std::string& ExternalDataName : Config.ExternalGlobalData )
    {
        ExternalData.insert(StringUtf8ToWide(ExternalDataName));
    }
    for ( const std::string& ExternalNamespace : Config.ExternalNamespaces )
    {
        ExternalNamespaces.insert(StringUtf8ToWide(ExternalNamespace));
    }
    for ( const std::string& ExternalTemplateName : Config.ExternalTemplatePredeclarationWhitelist )
    {
        ExternalTemplatePredeclarationWhitelist.insert(StringUtf8ToWide(ExternalTemplateName));
    }

    for ( const ManualHeaderDefinition& HeaderDefinition : Config.HeaderOverrides )
    {
        const std::wstring HeaderName = StringUtf8ToWide(HeaderDefinition.HeaderName);
        HeaderOverrideNameToFilePath.insert({ HeaderName, HeaderDefinition.HeaderFilesystemPath });

        for ( const std::string& ContainedTypeName : HeaderDefinition.ContainedTypes )
        {
            HeaderOverridenTypeNameToHeaderName.insert({ StringUtf8ToWide(ContainedTypeName), HeaderName });
        }
    }

    for ( const TypeRemapDefinition& Definition : Config.TypeRemap )
    {
        const SymbolNameInfo ClassName = SymbolNameInfo::FromSymbolName( StringUtf8ToWide(Definition.OriginalTypeName) );
        const std::pair ClassNamespaceAndName( ClassName.SymbolScope, ClassName.LocalName );
        if ( !TypeSubstitutions.contains( ClassNamespaceAndName ) )
        {
            TypeSubstitutions.insert({ ClassNamespaceAndName, std::vector<TypeSubstitutionCandidate>() });
        }
        std::vector<TypeSubstitutionCandidate>& AllSubstitutes = TypeSubstitutions.find( ClassNamespaceAndName )->second;

        TypeTemplateArgumentContainer WildcardTemplateArguments;
        const bool bArgumentsParsed = TypeTemplateArgumentContainer::ParseTemplateArguments( ClassName.TemplateArguments, WildcardTemplateArguments, true );
        assert(bArgumentsParsed && L"Failed to parse original template arguments for a type remap");

        TypeSubstitutionCandidate SubstitutionCandidate;
        SubstitutionCandidate.FilterTemplateArguments = WildcardTemplateArguments;

        const SymbolNameInfo ReplacementName = SymbolNameInfo::FromSymbolName( StringUtf8ToWide(Definition.ReplacementTypeName) );

        TypeTemplateArgumentContainer ReplacementTemplateArguments;
        const bool bReplacementArgumentsParsed = TypeTemplateArgumentContainer::ParseTemplateArguments( ReplacementName.TemplateArguments, ReplacementTemplateArguments, true );
        assert(bReplacementArgumentsParsed && L"Failed to parse replacement template arguments for a type remap");

        SubstitutionCandidate.SubtitutedNamespace = ReplacementName.SymbolScope;
        SubstitutionCandidate.SubtitutedClassName = ReplacementName.LocalName;
        SubstitutionCandidate.SubstitutedArguments = ReplacementTemplateArguments;
        AllSubstitutes.push_back( SubstitutionCandidate );
    }
}

CComPtr<IDiaSymbol> HeaderGenerator::ResolveEnumByFullName(const std::wstring& InFullEnumName)
{
    if ( const auto Iterator = EnumerationByNameCache.find( InFullEnumName ); Iterator != EnumerationByNameCache.end() )
    {
        return Iterator->second;
    }
    const CComPtr<IDiaSymbol> ResultEnumeration = ResolveSymbolByFullNameNoCache( InFullEnumName, SymTagEnum );
    EnumerationByNameCache.insert({ InFullEnumName, ResultEnumeration });
    return ResultEnumeration;
}

CComPtr<IDiaSymbol> HeaderGenerator::ResolveUDTByFullName(const std::wstring& InFullClassName)
{
    if ( const auto Iterator = UDTsByNameCache.find( InFullClassName ); Iterator != UDTsByNameCache.end() )
    {
        return Iterator->second;
    }
    const CComPtr<IDiaSymbol> ResultUDT = ResolveSymbolByFullNameNoCache( InFullClassName, SymTagUDT );
    UDTsByNameCache.insert({ InFullClassName, ResultUDT });
    return ResultUDT;
}

CComPtr<IDiaSymbol> HeaderGenerator::ResolveTemplateInstantiation( const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& ArgumentContainer )
{
    const std::shared_ptr<TemplateInstantiationMap> AllInstantiations = FindAllTemplateInstantiationSymbols( TemplateName );

    // Attempt to find the instantiation matching the template arguments
    if ( const auto Iterator = AllInstantiations->find( ArgumentContainer ); Iterator != AllInstantiations->end() )
    {
        return Iterator->second;
    }
    // If we didn't find anything, print an error (unless we are allowed to fail)
    std::wcerr << L"Failed to find a template instantiation matching arguments for template " << TemplateName.ToString() << std::endl;
    return nullptr;
}

std::shared_ptr<TemplateInstantiationMap> HeaderGenerator::FindAllTemplateInstantiationSymbols( const SymbolNameInfo& TemplateName)
{
    // Attempt to resolve an existing cache entry first
    const std::wstring FullTemplateName = TemplateName.ToString( SymbolNameInfo::IncludeNamespace );
    if ( const auto Iterator = TemplatesByNameCache.find( FullTemplateName ); Iterator != TemplatesByNameCache.end() )
    {
        return Iterator->second;
    }

    // Look for the new cache entry!
    // Make a pattern out of the template name to use for lookups!
    const std::wstring TemplateInstantiationWildcard = StringPrintf(L"%s<*>", FullTemplateName.c_str());

    const std::shared_ptr<TemplateInstantiationMap> ResultTemplateInstantiations = std::make_shared<TemplateInstantiationMap>();

    const auto ParseTemplateInstantiation = [&]( const CComPtr<IDiaSymbol>& TemplateInstantiationSymbol )
    {
        const SymbolNameInfo SymbolName = SymbolNameInfo::FromSymbol( TemplateInstantiationSymbol );

        // Skip over symbols that are not actually template instantiations, but are nested template specializations. Because nsfRegularExpression is dumb, <....> can actually match across multiple different templates
        if ( SymbolName.SymbolScope != TemplateName.SymbolScope || SymbolName.LocalName != TemplateName.LocalName ) return;

        TypeTemplateArgumentContainer ArgumentContainer;
        const bool bTemplateArgumentsParsed = TypeTemplateArgumentContainer::ParseTemplateArguments( SymbolName.TemplateArguments, ArgumentContainer );

        assert( bTemplateArgumentsParsed && L"Failed to parse template instantiation arguments" );
        ResultTemplateInstantiations->insert({ ArgumentContainer, TemplateInstantiationSymbol });
    };

    // Attempt to resolve this template as a top level template first
    for ( DiaChildSymbolIterator It( ExecutableSymbol, SymTagUDT, TemplateInstantiationWildcard.c_str(), nsfRegularExpression ); It; ++It )
    {
        const CComPtr<IDiaSymbol> TemplateInstantiationSymbol = *It;
        ParseTemplateInstantiation( TemplateInstantiationSymbol );
    }

    // If we found some template instantiations, we are good
    if ( !ResultTemplateInstantiations->empty() )
    {
        TemplatesByNameCache.insert( {FullTemplateName, ResultTemplateInstantiations} );
        return ResultTemplateInstantiations;
    }

    // Assume it is a nested template instantiation, so we need to look up the outer scope first
    CComPtr<IDiaSymbol> SymbolSearchScope;
    std::wstring LocalSymbolName;
    if ( !ResolveScopeForFullName( FullTemplateName, SymbolSearchScope, LocalSymbolName ) )
    {
        std::wcerr << L"Failed to find template instantiations for template " << FullTemplateName << L" because it's outer search scope is not found" << std::endl;
        TemplatesByNameCache.insert( {FullTemplateName, ResultTemplateInstantiations} );
        return ResultTemplateInstantiations;
    }

    // Attempt to search for all instantiations again, now with a correct scope
    for ( DiaChildSymbolIterator It( SymbolSearchScope, SymTagUDT, TemplateInstantiationWildcard.c_str(), nsfRegularExpression ); It; ++It )
    {
        const CComPtr<IDiaSymbol> TemplateInstantiationSymbol = *It;
        ParseTemplateInstantiation( TemplateInstantiationSymbol );
    }

    // Print a warning if we have not found a single template instantiation
    if ( !ResultTemplateInstantiations->empty() )
    {
        std::wcerr << L"Failed to find any template instantiations for template " << FullTemplateName << L" inside of it's outer search scope" << std::endl;
    }
    TemplatesByNameCache.insert( {FullTemplateName, ResultTemplateInstantiations} );
    return ResultTemplateInstantiations;
}

bool HeaderGenerator::ResolveScopeForFullName(const std::wstring& InFullName, CComPtr<IDiaSymbol>& OutScopeSymbol, std::wstring& OutNameInsideScope) const
{
    // More complex case - symbol is nested into one or more UDTs. Attempt to walk them in the reverse order
    std::wstring RemainingRootPath = InFullName;
    CComPtr<IDiaSymbol> TopLevelUserDefinedType;
    std::vector<std::wstring> PathToEnum;

    // Attempt to resolve the outermost UDT
    while ( TopLevelUserDefinedType == nullptr )
    {
        const size_t LastScopeSeparatorIndex = RemainingRootPath.rfind(L"::");

        // If this is the last path segment, then there is no enum with such name in the header generator
        if ( LastScopeSeparatorIndex == std::wstring::npos )
        {
            std::wcerr << L"Failed to resolve search scope for symbol " << InFullName << std::endl;
            return false;
        }

        // Remaining path is the name of the UDT, and right segment is the local symbol name
        PathToEnum.insert(PathToEnum.begin(), RemainingRootPath.substr(LastScopeSeparatorIndex + 1));
        RemainingRootPath = RemainingRootPath.substr(0, LastScopeSeparatorIndex);

        // Attempt to resolve the UDT from the remaining root path, assuming it is a top level UDT
        for ( DiaChildSymbolIterator It( ExecutableSymbol, SymTagUDT, RemainingRootPath.c_str() ); It; ++It )
        {
            TopLevelUserDefinedType = *It;
            break;
        }
    }

    // Recurse into the UDTs until we find the direct parent UDT of the symbol
    CComPtr<IDiaSymbol> CurrentSymbol = TopLevelUserDefinedType;
    continueOuterLoop:
    for ( int32_t i = 0; i < PathToEnum.size() - 1; i++ )
    {
        for ( DiaChildSymbolIterator It( CurrentSymbol, SymTagUDT, PathToEnum[i].c_str() ); It; ++It )
        {
            CurrentSymbol = *It;
            goto continueOuterLoop;
        }
        // We failed to resolve the nested UDT
        std::wcerr << L"Failed to resolve nested UDT " << PathToEnum[i] << L" while resolving search scope for symbol " << InFullName << std::endl;
        return false;
    }

    // We found a symbol that is the direct outer of our symbol!
    OutScopeSymbol = CurrentSymbol;
    OutNameInsideScope = PathToEnum[PathToEnum.size() - 1];
    return true;
}

CComPtr<IDiaSymbol> HeaderGenerator::ResolveSymbolByFullNameNoCache(const std::wstring& InFullSymbolName, const enum SymTagEnum InSymbolTag) const
{
     // Simplest case - symbol is top level, not nested into any UDT
    for ( DiaChildSymbolIterator It( ExecutableSymbol, InSymbolTag, InFullSymbolName.c_str() ); It; ++It )
    {
        const CComPtr<IDiaSymbol> TopLevelSymbol = *It;
        return TopLevelSymbol;
    }

    // Otherwise, we need to attempt to resolve the outer scope for the symbol
    CComPtr<IDiaSymbol> SymbolSearchScope;
    std::wstring LocalSymbolName;
    if ( !ResolveScopeForFullName( InFullSymbolName, SymbolSearchScope, LocalSymbolName ) )
    {
        std::wcerr << L"Failed to resolve symbol " << InFullSymbolName << L" because it's search scope could not be resolved" << std::endl;
        return nullptr;
    }

    // Lookup the symbol inside of it's search scope
    for ( DiaChildSymbolIterator It( SymbolSearchScope, InSymbolTag, LocalSymbolName.c_str() ); It; ++It )
    {
        const CComPtr<IDiaSymbol> EnumerationSymbol = *It;
        return EnumerationSymbol;
    }
    std::wcerr << L"Failed to resolve nested symbol inside of the UDT type chain: " << InFullSymbolName << std::endl;
    return nullptr;
}

bool HeaderGenerator::IsExternalUserDefinedType(const CComPtr<IDiaSymbol>& InTypeSymbol) const
{
    // Strip any CV modifiers from the UDT type
    const CComPtr<IDiaSymbol> OriginalType = DiaUtils::RemoveCVModifiersFromType( InTypeSymbol );

    // Fast path, if type ID is in the external type map, we can return true and skip any unnecessary calculations based on name or parent class
    const DWORD UniqueTypeId = GET_SYMBOL_ATTRIBUTE_CHECKED( OriginalType, symIndexId );
    if ( ExternalUserDefinedTypes.contains( UniqueTypeId ) )
    {
        return true;
    }

    // Check if this is a nested type, and lookup if it's class parent is external type as well
    if (const CComPtr<IDiaSymbol> TypeClassParent = const_cast<HeaderGenerator*>(this)->FindNestedTypeParent( InTypeSymbol ); TypeClassParent && IsExternalUserDefinedType( TypeClassParent ) )
    {
        return true;
    }

    // If this is a template instantiation, look if the entire template is marked as external
    const SymbolNameInfo SymbolNameInfo = SymbolNameInfo::FromSymbol( InTypeSymbol );
    if ( SymbolNameInfo.bIsTemplateInstantiation && ExternalTemplates.contains( SymbolNameInfo.ToString( SymbolNameInfo::IncludeNamespace ) ) )
    {
        return true;
    }
    // If this type comes from the standard library namespace, it is certainly external.
    // TODO: There are exclusions from this rule. For example, std::hash specializations should always be generated.
    if ( SymbolNameInfo.SymbolScope == L"std" || SymbolNameInfo.SymbolScope.starts_with(L"std::") )
    {
        return true;
    }
    return false;
}

bool HeaderGenerator::RegisterExternalUserDefinedType(const CComPtr<IDiaSymbol>& InExternalSymbol)
{
    // Strip any CV modifiers from the UDT type
    const CComPtr<IDiaSymbol> OriginalType = DiaUtils::RemoveCVModifiersFromType( InExternalSymbol );
    const DWORD UniqueTypeId = GET_SYMBOL_ATTRIBUTE_CHECKED( OriginalType, symIndexId );

    // If insert returns false, we already have this type registered, and do not need to do any extra work
    if ( !ExternalUserDefinedTypes.insert( UniqueTypeId ).second )
    {
        return false;
    }
    const SymbolNameInfo SymbolNameInfo = SymbolNameInfo::FromSymbol( InExternalSymbol );

    // If this is a template instantiation, the entire template is an external type
    // TODO: There are exclusions from this rule. For example, std::hash specializations should always be generated.
    if ( SymbolNameInfo.bIsTemplateInstantiation )
    {
        ExternalTemplates.insert( SymbolNameInfo.ToString( SymbolNameInfo::IncludeNamespace ) );
    }
    ExternalUserDefinedTypes.insert( UniqueTypeId );
    return true;
}

void HeaderGenerator::RegisterExternalTemplate(const SymbolNameInfo& TemplateName)
{
    if ( TemplateName.bIsTemplateInstantiation )
    {
        ExternalTemplates.insert( TemplateName.ToString( SymbolNameInfo::IncludeNamespace ) );
    }
}

void HeaderGenerator::RegisterExternalSymbolByRVA(const CComPtr<IDiaSymbol>& InSymbol)
{
    const DWORD SymbolTag = GET_SYMBOL_ATTRIBUTE_CHECKED( InSymbol, symTag );
    assert( SymbolTag == SymTagFunction || SymbolTag == SymTagData );

    const DWORD SymbolLocationType = GET_SYMBOL_ATTRIBUTE_CHECKED( InSymbol, locationType );
    if ( SymbolLocationType == LocIsStatic )
    {
        const DWORD SymbolRelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_CHECKED( InSymbol, relativeVirtualAddress );
        ExternalFunctionsAndDataRVAs.insert( SymbolRelativeVirtualAddress );
    }
}

const PublicSymbolInfo* HeaderGenerator::FindPublicSymbolByRVA(const DWORD RelativeVirtualAddress) const
{
    if ( const auto Iterator = PublicSymbolsByRVA.find(RelativeVirtualAddress); Iterator != PublicSymbolsByRVA.end() )
    {
        return &Iterator->second;
    }
    return nullptr;
}

CComPtr<IDiaSymbol> HeaderGenerator::FindNestedTypeParent(const CComPtr<IDiaSymbol>& InNestedType)
{
    const DWORD UniqueTypeId = GET_SYMBOL_ATTRIBUTE_CHECKED( InNestedType, symIndexId );

    // Attempt to retrieve an existing entry from the cache
    if ( const auto Iterator = NestedTypeParentCache.find( UniqueTypeId ); Iterator != NestedTypeParentCache.end() )
    {
        return Iterator->second;
    }

    // Attempt symbol name based lookup
    const SymbolNameInfo SymbolName = SymbolNameInfo::FromSymbol( InNestedType );

    // If symbol scope is empty. This is a top level symbol
    if ( SymbolName.SymbolScope.empty() )
    {
        NestedTypeParentCache.insert({ UniqueTypeId, nullptr });
        return nullptr;
    }

    // Attempt to lookup symbol scope as a class parent. Since namespaces cannot be nested inside of the UDTs, we do not need to try and backtrack
    // This might fail if parent symbol represents a template instantiation, but this is much faster than manually matching all template instantiations against the arguments, so try it
    for ( DiaChildSymbolIterator It( ExecutableSymbol, SymTagUDT, SymbolName.SymbolScope.c_str() ); It; ++It )
    {
        // Found a parent symbol, add it into the cache and return
        const CComPtr<IDiaSymbol> ParentTypeSymbol = *It;
        NestedTypeParentCache.insert({ UniqueTypeId, ParentTypeSymbol });
        return ParentTypeSymbol;
    }

    // Attempt to parse the parent scope as a symbol name. This is needed in case this is a template instantiation
    const SymbolNameInfo ParentSymbolName = SymbolNameInfo::FromSymbolName( SymbolName.SymbolScope );

    // Parent name is a template instantiation. We have to resolve the template arguments to determine the parent type,
    // because we failed to find the instantiation from the name as-is above. This is extremely slow, but we have to resort to this logic as a fallback in case fast path has failed.
    if ( ParentSymbolName.bIsTemplateInstantiation )
    {
        std::wcerr << L"Found a type nested inside of the template instantiation. This will require a very slow template instantiation lookup! Type: " << SymbolName.OriginalFullName << std::endl;

        TypeTemplateArgumentContainer TemplateArguments;
        const bool bParsedTemplateArguments = TypeTemplateArgumentContainer::ParseTemplateArguments( ParentSymbolName.TemplateArguments, TemplateArguments );
        assert( bParsedTemplateArguments && L"Failed to parse template instantiation arguments while resolving a parent symbol for a nested UDT/enum" );

        const CComPtr<IDiaSymbol> TemplateInstantiationSymbol = ResolveTemplateInstantiation( ParentSymbolName, TemplateArguments );
        assert( TemplateInstantiationSymbol && L"Failed to resolve template instantiation symbol for a nested UDT/enum" );

        NestedTypeParentCache.insert({ UniqueTypeId, TemplateInstantiationSymbol });
        return TemplateInstantiationSymbol;
    }

    // Found nothing, this is a top level symbol inside of the namespace
    NestedTypeParentCache.insert({ UniqueTypeId, nullptr });
    return nullptr;
}

bool HeaderGenerator::RemapTypeReference(const std::wstring& ClassNamespace, const std::wstring& ClassName, const TypeTemplateArgumentContainer& TypeArguments, std::wstring& OutClassReplacementNamespace, std::wstring& OutReplacementClassName, TypeTemplateArgumentContainer& OutReplacementTypeArguments) const
{
    const std::pair ClassNamespaceAndNamePair( ClassNamespace, ClassName );
    if ( const auto Iterator = TypeSubstitutions.find( ClassNamespaceAndNamePair ); Iterator != TypeSubstitutions.end() )
    {
        for ( const TypeSubstitutionCandidate& Candidate : Iterator->second )
        {
            TypeDeclarationMatchContext MatchContext;
            if ( Candidate.FilterTemplateArguments.Match( TypeArguments, MatchContext ) )
            {
                OutClassReplacementNamespace = Candidate.SubtitutedNamespace;
                OutReplacementClassName = Candidate.SubtitutedClassName;
                OutReplacementTypeArguments = Candidate.SubstitutedArguments.Substitute( MatchContext );
                return true;
            }
        }
    }
    return false;
}

bool HeaderGenerator::DoTemplateArgumentsNeedFullDefinition(const SymbolNameInfo& TemplateName) const
{
    // External templates need full definition, unless they are specifically whitelisted
    return ShouldMarkSymbolAsExternal( TemplateName, EExternalSymbolType::Type ) && !ExternalTemplatePredeclarationWhitelist.contains( TemplateName.ToString( SymbolNameInfo::IncludeNamespace ) );
}

bool HeaderGenerator::IsExternalSymbolByRVA(const DWORD RelativeVirtualAddress) const
{
    return ExternalFunctionsAndDataRVAs.contains(RelativeVirtualAddress);
}

void HeaderGenerator::PushDefinedSymbol(const std::wstring& InFullSymbolName)
{
    const size_t SymbolNameHash = std::hash<std::wstring>()( InFullSymbolName );
    const bool bInsertedNewElement = AllDefinedSymbolNameHashes.insert( SymbolNameHash ).second;

    if ( !bInsertedNewElement && !DuplicateSymbolDefinitions.contains( InFullSymbolName ) )
    {
        std::wcout << L"Found duplicate symbol " << InFullSymbolName  << L" defined across multiple translation units with the same name. It will be generated only once and marked as inline" << std::endl;
        DuplicateSymbolDefinitions.insert( InFullSymbolName );
    }
}

bool HeaderGenerator::IsDuplicateSymbolDefinition(const std::wstring& InFullSymbolName) const
{
    return DuplicateSymbolDefinitions.contains( InFullSymbolName );
}

bool HeaderGenerator::ShouldMarkSymbolAsExternal(const SymbolNameInfo& SymbolName, const EExternalSymbolType SymbolType) const
{
    const std::wstring SymbolNameWithNamespace = SymbolName.ToString( SymbolNameInfo::IncludeNamespace );

    // Check for explicit match with the name. Look up different maps depending on the symbol type
    if ( ( ( SymbolType & EExternalSymbolType::Type ) != EExternalSymbolType::None && ExternalTypeToHeaderLookup.contains( SymbolNameWithNamespace ) ) ||
        ( ( SymbolType & EExternalSymbolType::Data ) != EExternalSymbolType::None && ExternalData.contains( SymbolNameWithNamespace ) ) ||
        ( ( SymbolType & EExternalSymbolType::Function ) != EExternalSymbolType::None && ExternalFunctions.contains( SymbolNameWithNamespace ) ) )
    {
        return true;
    }
    // Check for a namespace match without a header definition.
    for (const std::wstring& Namespace : ExternalNamespaces)
    {
        if ( SymbolName.SymbolScope.starts_with( Namespace ) && ( SymbolName.SymbolScope.size() == Namespace.size() || SymbolName.SymbolScope[Namespace.size()] == L':' ) )
        {
            return true;
        }
    }
    // Check external namespace to header lookup, they are really only useful for types but can still allow to filter out chunks of code as fallback
    for (const std::wstring& Namespace : ExternalNamespaceToHeaderFallback | std::views::keys)
    {
        if ( SymbolName.SymbolScope.starts_with( Namespace ) && ( SymbolName.SymbolScope.size() == Namespace.size() || SymbolName.SymbolScope[Namespace.size()] == L':' ) )
        {
            return true;
        }
    }
    // C++ standard library is always considered external, even if we do not have a specific header to include
    return SymbolName.SymbolScope.starts_with(L"std");
}

const std::wstring* HeaderGenerator::FindExternalHeaderForType(const SymbolNameInfo& SymbolName) const
{
    // Try to resolve an individual type first
    if ( const auto Iterator = ExternalTypeToHeaderLookup.find( SymbolName.ToString( SymbolNameInfo::IncludeNamespace ) ); Iterator != ExternalTypeToHeaderLookup.end() )
    {
        return &Iterator->second;
    }
    // Fallback to namespace-based lookup
    for ( const auto& [Namespace, HeaderName] : ExternalNamespaceToHeaderFallback )
    {
        if ( SymbolName.SymbolScope.starts_with( Namespace ) )
        {
            return &HeaderName;
        }
    }
    return nullptr;
}

UserDefinedTypeInfo* HeaderGenerator::FindOrAddUserDefinedType(const CComPtr<IDiaSymbol>& ClassSymbol)
{
    // Strip any CV modifiers from the UDT type
    const CComPtr<IDiaSymbol> OriginalType = DiaUtils::RemoveCVModifiersFromType( ClassSymbol );
    const DWORD UniqueTypeId = GET_SYMBOL_ATTRIBUTE_CHECKED( OriginalType, symIndexId );

    if ( const auto Iterator = UserDefinedTypesLookup.find( UniqueTypeId ); Iterator != UserDefinedTypesLookup.end() )
    {
        return Iterator->second;
    }

    assert( !IsExternalUserDefinedType( OriginalType ) );
    const std::shared_ptr<UserDefinedTypeInfo> NewUserDefinedType = std::make_shared<UserDefinedTypeInfo>( this, OriginalType );
    assert( !ShouldMarkSymbolAsExternal( NewUserDefinedType->GetSymbolName(), EExternalSymbolType::Type ) );

    AllUserDefinedTypes.push_back(NewUserDefinedType);
    UserDefinedTypesLookup.insert({UniqueTypeId, NewUserDefinedType.get()});

    return NewUserDefinedType.get();
}

UserDefinedTypeInfo* HeaderGenerator::FindUserDefinedType(const CComPtr<IDiaSymbol>& ClassSymbol) const
{
    // Strip any CV modifiers from the UDT type
    const CComPtr<IDiaSymbol> OriginalType = DiaUtils::RemoveCVModifiersFromType( ClassSymbol );
    const DWORD UniqueTypeId = GET_SYMBOL_ATTRIBUTE_CHECKED( OriginalType, symIndexId );

    if ( const auto Iterator = UserDefinedTypesLookup.find( UniqueTypeId ); Iterator != UserDefinedTypesLookup.end() )
    {
        return Iterator->second;
    }
    return nullptr;
}

EnumerationInfo* HeaderGenerator::FindOrAddEnum( const CComPtr<IDiaSymbol>& EnumSymbol )
{
    // Strip any CV modifiers from the enum
    const CComPtr<IDiaSymbol> OriginalEnum = DiaUtils::RemoveCVModifiersFromType( EnumSymbol );
    const DWORD UniqueEnumId = GET_SYMBOL_ATTRIBUTE_CHECKED( OriginalEnum, symIndexId );

    if ( const auto Iterator = EnumLookup.find( UniqueEnumId ); Iterator != EnumLookup.end() )
    {
        return Iterator->second;
    }

    assert( !IsExternalUserDefinedType( OriginalEnum ) );
    const std::shared_ptr<EnumerationInfo> NewEnumInfo = std::make_shared<EnumerationInfo>( this, OriginalEnum );

    AllEnums.push_back(NewEnumInfo);
    EnumLookup.insert({UniqueEnumId, NewEnumInfo.get()});

    return NewEnumInfo.get();
}

GeneratedHeaderFile* HeaderGenerator::FindOrCreateHeaderFile(const std::wstring& HeaderFilename)
{
    if ( const auto Iterator = GeneratedHeaderFilesLookup.find( HeaderFilename ); Iterator != GeneratedHeaderFilesLookup.end() )
    {
        return Iterator->second;
    }

    const std::shared_ptr<GeneratedHeaderFile> NewHeaderFile = std::make_shared<GeneratedHeaderFile>( this, HeaderFilename );
    AllGeneratedHeaderFiles.push_back( NewHeaderFile );
    GeneratedHeaderFilesLookup.insert({HeaderFilename, NewHeaderFile.get()});

    return NewHeaderFile.get();
}

GeneratedHeaderFile* HeaderGenerator::FindHeaderFileForSymbol(const CComPtr<IDiaSymbol>& InSymbol)
{
    // We only associate top level symbols with header files
    const CComPtr<IDiaSymbol> TopLevelUnmodifiedSymbol = FindTopLevelType( DiaUtils::RemoveCVModifiersFromType( InSymbol ) );
    const DWORD TopLevelSymbolIndexId = GET_SYMBOL_ATTRIBUTE_CHECKED( TopLevelUnmodifiedSymbol, symIndexId );

    if (const auto HeaderFileEntry = SymbolHeaderFiles.find( TopLevelSymbolIndexId ); HeaderFileEntry != SymbolHeaderFiles.end() )
    {
        return HeaderFileEntry->second;
    }
    return nullptr;
}

void HeaderGenerator::AssociateSymbolWithHeaderFile(const CComPtr<IDiaSymbol>& InSymbol, GeneratedHeaderFile* InHeaderFile)
{
    // We only associate top level symbols with header files
    const DWORD SymbolIndexId = GET_SYMBOL_ATTRIBUTE_CHECKED( InSymbol, symIndexId );

    SymbolHeaderFiles.insert({SymbolIndexId, InHeaderFile});
}

void HeaderGenerator::CreateShortImportLibrary()
{
    std::wcout << L"Creating import library to link against" << std::endl;

    std::string DllNameAsciiOnly;
    for ( const wchar_t Character : DllName )
    {
        DllNameAsciiOnly.push_back( toascii(Character) );
    }
    const DWORD MachineType = GET_SYMBOL_ATTRIBUTE_CHECKED( ExecutableSymbol, machineType );
    COFFImportLibrary ImportLibrary( DllNameAsciiOnly, MachineType );

    // Iterate the public symbols and build import archive members for them
    uint64_t TotalNumberOfImportsWritten = 0;
    for ( DiaChildSymbolIterator PublicSymbolIt( ExecutableSymbol, SymTagPublicSymbol ); PublicSymbolIt; ++PublicSymbolIt )
    {
        const CComPtr<IDiaSymbol> PublicSymbol = *PublicSymbolIt;
        const DWORD LocationType = GET_SYMBOL_ATTRIBUTE_CHECKED( PublicSymbol, locationType );
        assert( LocationType == LocIsStatic );
        const DWORD RelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( PublicSymbol, relativeVirtualAddress );

        // Skip some internal symbols with zero RVA. The ones I noticed are __AbsoluteZero and a bunch of others
        if ( RelativeVirtualAddress == 0 ) continue;
        // Do not export any internal symbols or symbols that we consider external
        // Checks are ordered from fastest to slowest, since there are a lot of public symbols even in small DLLs
        if ( IsExternalSymbolByRVA(RelativeVirtualAddress) ) continue;

        // Check if the undecorated symbol name is considered external. This is quite a bit slower but fewer functions should qualify for this
        const SymbolNameInfo UndecoratedName = SymbolNameInfo::FromSymbolName( DiaUtils::GetSymbolUndecoratedName( PublicSymbol, UNDNAME_NAME_ONLY ) );
        const BOOL bIsSymbolCode = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( PublicSymbol, code );
        if ( CodeGeneration::IsInternalSymbolName( UndecoratedName ) || ShouldMarkSymbolAsExternal( UndecoratedName, bIsSymbolCode ? EExternalSymbolType::Function : EExternalSymbolType::Data ) ) continue;

        // "extern C" linkage symbols will have a symbol name that does not start with a question mark, which is mangling prefix used by MSVC
        const std::wstring DecoratedSymbolName = GET_SYMBOL_ATTRIBUTE_CHECKED( PublicSymbol, name );
        const bool bIsCLinkage = !DecoratedSymbolName.starts_with(L'?');

        // Append the entry for this public symbol into the map
        PublicSymbolInfo SymbolInfo;
        SymbolInfo.bIsCLinkage = bIsCLinkage;
        PublicSymbolsByRVA.insert({ RelativeVirtualAddress, SymbolInfo });

        std::string SymbolNameAsciiOnly;
        SymbolNameAsciiOnly.append(DecoratedSymbolName.size(), '\0');
        for ( int32_t CharacterIndex = 0; CharacterIndex < DecoratedSymbolName.size(); CharacterIndex++ )
        {
            SymbolNameAsciiOnly[CharacterIndex] = toascii(DecoratedSymbolName[CharacterIndex]);
        }
        ImportLibrary.AddImport( SymbolNameAsciiOnly, bIsSymbolCode );
        TotalNumberOfImportsWritten++;
    }

    // Write the resulting import library into the file (matching the name of the DLL but with a different extension)
    const std::filesystem::path ImportLibraryPath = (OutputDirectoryPath.parent_path() / DllName).replace_extension(L".lib");
    const bool bLibraryWritten = ImportLibrary.WriteLibrary( ImportLibraryPath.wstring() );
    assert( bLibraryWritten && L"Failed to write import library into the file" );

    std::wcout << L"Import Library written to: " << ImportLibraryPath.wstring() << " (" << TotalNumberOfImportsWritten << L" imports)" << std::endl;
}

bool HeaderGenerator::IsFunctionGeneratedThunk( const CComPtr<IDiaSymbol>& FunctionSymbol )
{
    const CComPtr<IDiaSymbol> FunctionSignatureTypeSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionSymbol, type );
    const DWORD FunctionSignatureSymbolTag = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSignatureTypeSymbol, symTag );

    // Thunk functions have BaseType as their type with btNoType value
    if ( FunctionSignatureSymbolTag == SymTagBaseType )
    {
        const DWORD BaseFunctionType = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionSignatureTypeSymbol, baseType );
        assert( BaseFunctionType == btNoType );
        return true;
    }
    assert( FunctionSignatureSymbolTag == SymTagFunctionType && L"Function type should either be a btNoType or a function type" );
    return false;
}

void HeaderGenerator::DiscoverAllCompilands()
{
    std::wcout << L"Discovering contents of compilands..." << std::endl;

    for ( DiaChildSymbolIterator It( ExecutableSymbol, SymTagCompiland ); It; ++It )
    {
        const CComPtr<IDiaSymbol> CompilandSymbol = *It;
        if ( const std::shared_ptr<CompilationUnit> CompilationUnit = CreateCompilationUnitForCompiland( CompilandSymbol ) )
        {
            AllCompilationUnits.push_back( CompilationUnit );
        }
    }
}

void HeaderGenerator::DiscoverAllSymbols( int32_t& CurrentActionNumber )
{
    std::wcout << L"Searching for symbols not mentioned in the compilands..." << std::endl;

    const std::wstring GlobalSymbolName = GET_SYMBOL_ATTRIBUTE_CHECKED( ExecutableSymbol, name );
    const std::shared_ptr<CompilationUnit> GlobalLibraryCompilationUnit = std::make_shared<CompilationUnit>( this, ExecutableSymbol, GlobalSymbolName, false );
    GlobalLibraryCompilationUnit->bIsGlobalScopeUnit = true;

    AllCompilationUnits.push_back( GlobalLibraryCompilationUnit );

    for ( DiaChildSymbolIterator It( ExecutableSymbol, SymTagData ); It; ++It )
    {
        CComPtr<IDiaSymbol> Symbol = *It;

        // Skip compiler generated data symbols, like virtual function tables or initializers
        if ( GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( Symbol, compilerGenerated ) ) continue;

        // Not interested in symbols with non-static location
        const DWORD SymbolLocationType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( Symbol, locationType );
        if ( SymbolLocationType != LocIsStatic ) continue;

        // Not interested in symbols parented to compilands or functions
        const CComPtr<IDiaSymbol> LexicalParentSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( Symbol, lexicalParent );
        const DWORD LexicalParentSymTag = GET_SYMBOL_ATTRIBUTE_CHECKED( LexicalParentSymbol, symTag );
        if ( LexicalParentSymTag == SymTagCompiland || LexicalParentSymTag == SymTagFunction ) continue;

        // Skip data already discovered as a part of other compilands
        const SymbolNameInfo DataName = SymbolNameInfo::FromSymbol( Symbol );
        const size_t SymbolNameHash = std::hash<std::wstring>()( DataName.OriginalFullName );
        if ( AllDefinedSymbolNameHashes.contains( SymbolNameHash ) ) continue;

        // Skip data we can infer to be compiler generated as well
        if ( CodeGeneration::IsInternalSymbolName( DataName ) ) continue;
        // Skip external data
        if ( ShouldMarkSymbolAsExternal( DataName, EExternalSymbolType::Data ) ) continue;
        // Here we additionally skip any symbols starting with an underscore, as we consider them part of standard library
        if ( DataName.LocalName.starts_with(L"_") ) continue;

        std::wcerr << L"Symbol " << DataName.OriginalFullName << L" has global scope as it's parent, and not a compiland. This should never happen!" << std::endl;

        // Skip variables nested into classes
        const CComPtr<IDiaSymbol> ParentTypeSymbol = FindNestedTypeParent( Symbol );
        if ( ParentTypeSymbol != nullptr ) continue;

        // We should never find UDT data members here
        if ( const CComPtr<IDiaSymbol> ClassParent = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(Symbol, classParent) )
        {
            assert(!L"Found data member embedded into the UDT that does not belong to any compiland. This should not be possible");
            continue;
        }

        // TODO: This actually results in local variables having the same name but different type not be generated. There have been examples of that with variable named 'singleton'
        AllDefinedSymbolNameHashes.insert( SymbolNameHash );
        GlobalLibraryCompilationUnit->GlobalVariables.push_back( Symbol );
        GlobalLibraryCompilationUnit->AllDataMemberDefinitions.push_back( Symbol );
    }

    GlobalLibraryCompilationUnit->ProcessActions_Pass1( CurrentActionNumber );
    GlobalLibraryCompilationUnit->ProcessActions_Pass2( CurrentActionNumber );
}

std::shared_ptr<CompilationUnit> HeaderGenerator::CreateCompilationUnitForCompiland(const CComPtr<IDiaSymbol>& CompilandSymbol)
{
    const std::wstring LibraryFilePathString = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(CompilandSymbol, libraryName);

    // Skip compilands with no library name. Compilands like * Linker *, * CIL * and * Linker Generated Manifest RES * will have no library name,
    // and they described generated compilands or metadata that we are not actively interested in
    if ( LibraryFilePathString.empty() )
    {
        return nullptr;
    }

    const std::wstring SymbolName = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT(CompilandSymbol, name);
    const std::filesystem::path LibraryFileName = std::filesystem::path( LibraryFilePathString ).filename();
    const std::filesystem::path Extension = LibraryFileName.extension();

    // Skip .exp files because they have nothing in them and there should always be only one export file for module
    if ( Extension == L".exp" )
    {
        return nullptr;
    }

    // .lib compilands represent objects linked into either statically linked or dynamically linked libraries
    // For example, Visual C++ runtime could be statically linked, and KERNEL32.dll could be dynamically linked
    // Either way, we are not very interested in .lib compilands other than keeping track of the linked libraries
    if ( Extension == L".lib" )
    {
        // If symbol name ends with .dll, that means this lib is linking against a DLL. Otherwise, it's linking agaisnt an static library
        // DLLs will get two compilands: one with symbol name matching the DLL name and the other matching the DLL name + Import: prefix
        if ( SymbolName.ends_with(L".dll") )
        {
            std::wstring DllName = SymbolName;
            if ( DllName.starts_with(L"Import:") )
            {
                DllName.erase(0, 7);
            }
            // Create object for a dynamically linked library to collect types that this library defines
            return std::make_shared<CompilationUnit>( this, CompilandSymbol, SymbolName, true );
        }
        // Create proxy object for this statically linked library to collect types that the library uses
        if ( SymbolName.ends_with(L".obj") )
        {
            return std::make_shared<CompilationUnit>( this, CompilandSymbol, SymbolName, true );
        }
        return nullptr;
    }

    // .obj library name represents naked objects being linked into the current library, without being wrapped into a .lib file
    // These are the objects that we consider the code that is actually owned by the current PDB, and that we want to emit.
    // Keep the names of the objects for later use
    if ( Extension == L".obj" )
    {
        // There should never be more than ONE object linked into an individual object file
        assert( SymbolName == LibraryFilePathString );

        const std::wstring CompilationUnitName = std::filesystem::path(LibraryFileName).replace_extension().wstring();
        return std::make_shared<CompilationUnit>( this, CompilandSymbol, CompilationUnitName, false );
    }
    std::wcout << L"Unknown compiland with library name that is not .lib or .obj: " << LibraryFilePathString << ". Symbol Name: " << SymbolName << std::endl;
    return nullptr;
}

void HeaderGenerator::Generate(const CComPtr<IDiaSymbol>& InGlobalSymbol)
{
    ExecutableSymbol = InGlobalSymbol;
    DiscoverAllCompilands();

    // Calculate total amount of actions for progress estimate
    TotalActionCount = 0;
    for (const std::shared_ptr<CompilationUnit>& CompilationUnit : AllCompilationUnits )
    {
        TotalActionCount += CompilationUnit->EstimateActionCount();
    }

    // Process external object files first to collect all external types
    int32_t CurrentActionNumber = 1;
    for (const std::shared_ptr<CompilationUnit>& CompilationUnit : AllCompilationUnits )
    {
        if ( CompilationUnit->IsExternalCompilationUnit() )
        {
            CompilationUnit->ProcessActions_Pass1( CurrentActionNumber );
        }
    }

    // Process our own object files now and register data from them
    for (const std::shared_ptr<CompilationUnit>& CompilationUnit : AllCompilationUnits )
    {
        if ( !CompilationUnit->IsExternalCompilationUnit() )
        {
            CompilationUnit->ProcessActions_Pass1( CurrentActionNumber );
        }
    }

    // Generate an import library and determine name mangling scheme for symbols
    CreateShortImportLibrary();

    // Associate object files data with header files, now that we know which symbols we consider duplicate
    for (const std::shared_ptr<CompilationUnit>& CompilationUnit : AllCompilationUnits )
    {
        CompilationUnit->ProcessActions_Pass2( CurrentActionNumber );
    }

    DiscoverAllSymbols( CurrentActionNumber );

    // Process enum actions first because they can allocate new types as their parent, but new types cannot create new enums
    for ( const std::shared_ptr<EnumerationInfo>& EnumInfo : AllEnums )
    {
        EnumInfo->ProcessActions();
    }
    // We have to use index-based interation here because new types might be created by attempting to associate nested types with header files
    for ( int32_t TypeIndex = 0; TypeIndex < AllUserDefinedTypes.size(); TypeIndex++ )
    {
        AllUserDefinedTypes[ TypeIndex ]->ProcessActions();
    }

    // Clear output directory
    std::filesystem::remove_all( OutputDirectoryPath );

    // Copy predefined header files into the output directory
    for ( const auto& [HeaderFileName, HeaderFilePath] : HeaderOverrideNameToFilePath )
    {
        const std::filesystem::path DestinationPath = std::filesystem::path(OutputDirectoryPath) / StringPrintf(L"%s.h", HeaderFileName.c_str());
        const bool bCopySuccessful = std::filesystem::copy_file( HeaderFilePath, DestinationPath );
        assert( bCopySuccessful && L"Failed to copy overriden header file from template directory to output directory" );
    }

    // Generate resulting header files
    for ( const std::shared_ptr<GeneratedHeaderFile>& HeaderFile : AllGeneratedHeaderFiles )
    {
        HeaderFile->GenerateCppFile();
    }
}

bool HeaderGenerator::IsHeaderFilenameOccupiedByManualHeader(const std::wstring &InHeaderFilename) const {
    return HeaderOverrideNameToFilePath.find(InHeaderFilename) != HeaderOverrideNameToFilePath.end();
}

const std::wstring *HeaderGenerator::FindOverridenManualHeaderForType(const SymbolNameInfo &ClassName) const {
    auto it = HeaderOverridenTypeNameToHeaderName.find(ClassName.OriginalFullName);
    if (it != HeaderOverridenTypeNameToHeaderName.end()) {
        return &it->second;
    }
    return nullptr;
}
