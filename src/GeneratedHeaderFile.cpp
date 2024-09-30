#include "GeneratedHeaderFile.h"
#include "CodeGeneration.h"
#include "AST/CppDeclarationTree.h"
#include "HeaderGenerator.h"
#include "TypeDependencyCollector.h"
#include <iostream>
#include <ranges>
#include <Utils/StringUtils.h>

GeneratedHeaderFile::GeneratedHeaderFile( HeaderGenerator* InOwnerGenerator, const std::wstring& InHeaderFileName ) : OwnerGenerator( InOwnerGenerator ), HeaderFileName( InHeaderFileName )
{
}

void GeneratedHeaderFile::AddTopLevelType(UserDefinedTypeInfo* InUserDefinedType)
{
    ContainedTopLevelTypes.push_back( InUserDefinedType );
    OwnerGenerator->AssociateSymbolWithHeaderFile( DiaUtils::RemoveCVModifiersFromType( InUserDefinedType->GetUDTSymbol() ), this );
}

void GeneratedHeaderFile::AddTopLevelEnumeration(EnumerationInfo* InEnumerationInfo)
{
    ContainedTopLevelEnums.push_back( InEnumerationInfo );
    OwnerGenerator->AssociateSymbolWithHeaderFile( DiaUtils::RemoveCVModifiersFromType( InEnumerationInfo->GetEnumSymbol() ), this );
}

void GeneratedHeaderFile::AddGlobalFunction(const CComPtr<IDiaSymbol>& InGlobalFunction)
{
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( InGlobalFunction, symTag ) == SymTagFunction );
    ContainedGlobalFunctions.push_back( InGlobalFunction );
    OwnerGenerator->AssociateSymbolWithHeaderFile( InGlobalFunction, this );
}

void GeneratedHeaderFile::AddGlobalVariable(const CComPtr<IDiaSymbol>& InGlobalVariable)
{
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( InGlobalVariable, symTag ) == SymTagData );
    ContainedGlobalVariables.push_back( InGlobalVariable );
    OwnerGenerator->AssociateSymbolWithHeaderFile( InGlobalVariable, this );
}

void GeneratedHeaderFile::GenerateCppFile()
{
    // TODO: Fetch data model from the executable file instead of assuming it's Windows x64 at all times
    GeneratedFile = std::make_unique<CppFile>( DataModel::EDataModel::LLP64, TypeFormattingRules::FixedLengthInt | TypeFormattingRules::Cstdint );
    GeneratedFile->FileName = HeaderFileName;
    GeneratedFile->bIsHeaderFile = true;

    PopulateCppFile();
    GeneratedFile->WriteToFile( OwnerGenerator->GetOutputDirectory() );
}

void GeneratedHeaderFile::PopulateCppFile() const
{
    std::unordered_map<IDiaSymbol*, ITopLevelDeclaration*> TopLevelDeclarationsBySymbol;

    // Generate top level data, functions and types
    for ( const CComPtr<IDiaSymbol>& GlobalData : ContainedGlobalVariables )
    {
        const SymbolNameInfo DataNameInfo = SymbolNameInfo::FromSymbol( GlobalData );

        // Skip lambda symbols
        if ( DataNameInfo.bIsLambdaSymbol ) continue;
        // Skip MSVC auto-generated internal functions such as __autoclassinit and __vecDelDtor
        if ( CodeGeneration::IsInternalSymbolName( DataNameInfo ) ) continue;

        const std::shared_ptr<GlobalDataDeclaration> GlobalDataDecl = MakeTopLevelData( GlobalData, OwnerGenerator );
        // GlobalDataDecl->bIsExternCLinkage = OwnerGenerator->IsSymbolExternCLinkage( GlobalData );
        GeneratedFile->Declarations.push_back( GlobalDataDecl );
        TopLevelDeclarationsBySymbol.insert({GlobalData.p, GlobalDataDecl.get()});
    }

    for ( const CComPtr<IDiaSymbol>& GlobalFunction : ContainedGlobalFunctions )
    {
        const SymbolNameInfo FunctionNameInfo = SymbolNameInfo::FromSymbol( GlobalFunction );

        // Skip lambda symbols
        if ( FunctionNameInfo.bIsLambdaSymbol ) continue;
        // Skip MSVC auto-generated internal functions such as __autoclassinit and __vecDelDtor
        if ( CodeGeneration::IsInternalSymbolName( FunctionNameInfo ) ) continue;

        const std::shared_ptr<GlobalFunctionDeclaration> GlobalFunctionDecl = MakeTopLevelFunction( GlobalFunction, OwnerGenerator );
        GeneratedFile->Declarations.push_back( GlobalFunctionDecl );
        TopLevelDeclarationsBySymbol.insert({GlobalFunction.p, GlobalFunctionDecl.get()});
    }

    for ( const UserDefinedTypeInfo* TopLevelUDT : ContainedTopLevelTypes )
    {
        std::wstring Namespace;
        const std::shared_ptr<ITopLevelDeclaration> TopLevelTypeDecl = std::make_shared<UDTDeclaration>( MakeUDT( TopLevelUDT->GetUDTSymbol(), TopLevelUDT, &Namespace, OwnerGenerator ) );
        TopLevelTypeDecl->Namespace = Namespace;
        GeneratedFile->Declarations.push_back( TopLevelTypeDecl );
        TopLevelDeclarationsBySymbol.insert({TopLevelUDT->GetUDTSymbol().p, TopLevelTypeDecl.get()});
    }

    for ( const EnumerationInfo* TopLevelEnum : ContainedTopLevelEnums )
    {
        std::wstring Namespace;
        const std::shared_ptr<ITopLevelDeclaration> EnumDecl = std::make_shared<EnumDeclaration>( MakeEnum( TopLevelEnum->GetEnumSymbol(), &Namespace, OwnerGenerator ) );
        EnumDecl->Namespace = Namespace;
        GeneratedFile->Declarations.push_back( EnumDecl );
        TopLevelDeclarationsBySymbol.insert({TopLevelEnum->GetEnumSymbol().p, EnumDecl.get()});
    }

    // Generate local and global dependencies for each of the functions
    HeaderFileReferenceCollector ReferenceCollector( this );
    ReferenceCollector.PopulateWithTopLevelDefinitions( TopLevelDeclarationsBySymbol );

    ReferenceCollector.PopulateGeneratedFileWithDependencies( *GeneratedFile );
}

std::shared_ptr<GlobalDataDeclaration> GeneratedHeaderFile::MakeTopLevelData(const CComPtr<IDiaSymbol>& InDataSymbol, ITypeResolutionProvider* TypeProvider)
{
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( InDataSymbol, symTag ) == SymTagData );
    const DWORD LocationType = GET_SYMBOL_ATTRIBUTE_CHECKED( InDataSymbol, locationType );
    const CComPtr<IDiaSymbol> DataTypeSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( InDataSymbol, type );
    const SymbolNameInfo SymbolNameInfo = SymbolNameInfo::FromSymbol( InDataSymbol );

    // Top level variables should either be static or TLS, never member variables or bitfields.
    assert( LocationType == LocIsStatic || LocationType == LocIsTLS || LocationType == LocIsConstant );

    std::shared_ptr<GlobalDataDeclaration> DataDeclaration = std::make_shared<GlobalDataDeclaration>();

    if ( LocationType == LocIsStatic )
    {
        const DWORD SymbolRelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_CHECKED( InDataSymbol, relativeVirtualAddress );

        // Check if this function has a C name mangling scheme
        if ( const PublicSymbolInfo* PublicSymbolData = TypeProvider->FindPublicSymbolByRVA( SymbolRelativeVirtualAddress ) )
        {
            DataDeclaration->bIsExternCLinkage = PublicSymbolData->bIsCLinkage;
        }
    }

    // We want the name without the namespace, but with template arguments
    DataDeclaration->Namespace = SymbolNameInfo.SymbolScope;
    DataDeclaration->DataName = SymbolNameInfo.LocalName;
    DataDeclaration->DataType = CodeGeneration::FormatTypeName( DataTypeSymbol, TypeProvider );
    DataDeclaration->bIsTemplateSpecialization = SymbolNameInfo.bIsTemplateInstantiation;
    DataDeclaration->bIsThreadLocal = LocationType == LocIsTLS;
    // Import the variable unless it is a constant declaration
    DataDeclaration->bIsDllImport = LocationType != LocIsConstant;

    if ( SymbolNameInfo.bIsTemplateInstantiation )
    {
        DataDeclaration->bIsTemplateSpecialization = true;
        const bool bTemplateArgumentsParsed = TypeTemplateArgument::ParseTemplateArguments( SymbolNameInfo.TemplateArguments, DataDeclaration->TemplateArguments );
        assert( bTemplateArgumentsParsed && L"Failed to parse template instantiation arguments" );
    }

    // Generate constant value
    if ( LocationType == LocIsConstant )
    {
        const CComVariant Variant = GET_SYMBOL_ATTRIBUTE_CHECKED( InDataSymbol, value );
        DataDeclaration->ConstantValue = CodeGeneration::GenerateConstantValue( Variant );
        DataDeclaration->bIsConstexpr = true;
    }
    return DataDeclaration;
}

std::shared_ptr<GlobalFunctionDeclaration> GeneratedHeaderFile::MakeTopLevelFunction(const CComPtr<IDiaSymbol>& InFunctionSymbol, ITypeResolutionProvider* TypeProvider)
{
    assert( GET_SYMBOL_ATTRIBUTE_CHECKED( InFunctionSymbol, symTag ) == SymTagFunction );
    const CComPtr<IDiaSymbol> FunctionTypeSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( InFunctionSymbol, type );
    const SymbolNameInfo SymbolNameInfo = SymbolNameInfo::FromSymbol( InFunctionSymbol );

    std::shared_ptr<GlobalFunctionDeclaration> FunctionDeclaration = std::make_shared<GlobalFunctionDeclaration>();

    const DWORD SymbolLocationType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InFunctionSymbol, locationType );
    assert( SymbolLocationType == LocIsStatic );
    const DWORD SymbolRelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_CHECKED( InFunctionSymbol, relativeVirtualAddress );

    // Check if this function has a C name mangling scheme
    if ( const PublicSymbolInfo* PublicSymbolData = TypeProvider->FindPublicSymbolByRVA( SymbolRelativeVirtualAddress ) )
    {
        FunctionDeclaration->bIsExternCLinkage = PublicSymbolData->bIsCLinkage;
    }
    // We want the name without the namespace, but with template arguments
    FunctionDeclaration->Namespace = SymbolNameInfo.SymbolScope;
    FunctionDeclaration->FunctionName = SymbolNameInfo.LocalName;
    FunctionDeclaration->bIsTemplateSpecialization = SymbolNameInfo.bIsTemplateInstantiation;
    FunctionDeclaration->bIsDllImport = true;

    if ( SymbolNameInfo.bIsTemplateInstantiation )
    {
        FunctionDeclaration->bIsTemplateSpecialization = true;
        const bool bTemplateArgumentsParsed = TypeTemplateArgument::ParseTemplateArguments( SymbolNameInfo.TemplateArguments, FunctionDeclaration->TemplateArguments );
        assert( bTemplateArgumentsParsed && L"Failed to parse template instantiation arguments" );
    }
    CodeGeneration::FormatFunctionType( FunctionTypeSymbol, TypeProvider, FunctionDeclaration->ReturnType, FunctionDeclaration->ParameterNamesAndTypes, FunctionDeclaration->bIsVariadicArguments, nullptr, InFunctionSymbol );

    return FunctionDeclaration;
}

std::shared_ptr<EnumDeclarationData> GeneratedHeaderFile::MakeEnum(const CComPtr<IDiaSymbol>& EnumTypeSymbol, std::wstring* OutEnumNamespace, ITypeResolutionProvider* TypeProvider)
{
    const SymbolNameInfo EnumNameInfo = SymbolNameInfo::FromSymbol( EnumTypeSymbol );

    const CComPtr<IDiaSymbol> UnderlyingType = GET_SYMBOL_ATTRIBUTE_CHECKED( EnumTypeSymbol, type );
    const BOOL bIsEnumScoped = GET_SYMBOL_ATTRIBUTE_CHECKED( EnumTypeSymbol, scoped );

    const std::shared_ptr<ITypeDeclaration> UnderlyingTypeDecl = CodeGeneration::FormatTypeName( UnderlyingType, TypeProvider );
    const std::shared_ptr<EnumDeclarationData> EnumDecl = std::make_shared<EnumDeclarationData>();

    EnumDecl->EnumName = EnumNameInfo.ToString( SymbolNameInfo::IncludeLocalNameOnly );
    EnumDecl->UnderlyingType = UnderlyingTypeDecl;
    // TODO: Assume true, but there is really no way to tell because scoped is actually extremely not descriptive and is not set on the enums that 100% should be scoped.
    EnumDecl->bIsScoped = true; //bIsEnumScoped;

    if ( OutEnumNamespace )
    {
        *OutEnumNamespace = EnumNameInfo.SymbolScope;
    }

    for ( DiaChildSymbolIterator It( EnumTypeSymbol, SymTagData ); It; ++It )
    {
        const CComPtr<IDiaSymbol> EnumConstantSymbol = *It;
        const SymbolNameInfo EnumConstantNameInfo = SymbolNameInfo::FromSymbol( EnumConstantSymbol );
        const CComVariant Variant = GET_SYMBOL_ATTRIBUTE_CHECKED( EnumConstantSymbol, value );

        uint64_t EnumConstantValueInt;
        if ( Variant.vt == VT_I1 || Variant.vt == VT_I2 || Variant.vt == VT_I4 || Variant.vt == VT_I8 || Variant.vt == VT_INT )
        {
            EnumConstantValueInt = static_cast<uint64_t>( Variant.vt == VT_I1 ? Variant.cVal : Variant.vt == VT_I2 ? Variant.iVal : Variant.vt == VT_I4 ? Variant.lVal : Variant.vt == VT_I8 ? Variant.llVal : Variant.intVal );
        }
        else if ( Variant.vt == VT_UI1 || Variant.vt == VT_UI2 || Variant.vt == VT_UI4 || Variant.vt == VT_UI8 || Variant.vt == VT_UINT )
        {
            EnumConstantValueInt = Variant.vt == VT_UI1 ? Variant.bVal : Variant.vt == VT_I2 ? Variant.uiVal : Variant.vt == VT_I4 ? Variant.ulVal : Variant.vt == VT_I8 ? Variant.ullVal : Variant.uintVal;
        }
        else
        {
            assert(!L"unsupported VARIANT type found as enumeration constant value");
            EnumConstantValueInt = 0;
        }
        EnumDecl->Values.push_back({EnumConstantNameInfo.LocalName, EnumConstantValueInt});
    }

    return EnumDecl;
}

std::shared_ptr<UDTDeclarationData> GeneratedHeaderFile::MakeUDT(const CComPtr<IDiaSymbol>& UDTSymbol, const UserDefinedTypeInfo* TypeInfo, std::wstring* OutTypeNamespace, ITypeResolutionProvider* TypeProvider)
{
    const DWORD UDTKind = GET_SYMBOL_ATTRIBUTE_CHECKED( UDTSymbol, udtKind );
    const SymbolNameInfo TypeNameInfo = SymbolNameInfo::FromSymbol( UDTSymbol );
    const std::shared_ptr<UDTDeclarationData> TypeDecl = std::make_shared<UDTDeclarationData>();

    // We want the template arguments, but do not want the namespace
    TypeDecl->ClassName = TypeNameInfo.LocalName;
    TypeDecl->Kind = CodeGeneration::ConvertUDTKind( UDTKind );

    if ( TypeNameInfo.bIsTemplateInstantiation )
    {
        TypeDecl->bIsTemplateSpecialization = true;
        const bool bTemplateArgumentsParsed = TypeTemplateArgument::ParseTemplateArguments( TypeNameInfo.TemplateArguments, TypeDecl->TemplateArguments );
        assert( bTemplateArgumentsParsed && L"Failed to parse template instantiation arguments" );
    }

    // Give namespace to the caller. This is only relevant for top level UDTs, nested UDTs will have their parent's full name
    if ( OutTypeNamespace )
    {
        *OutTypeNamespace = TypeNameInfo.SymbolScope;
    }

    // Generate base class list in the order of declaration
    for ( DiaChildSymbolIterator It( UDTSymbol, SymTagBaseClass ); It; ++It )
    {
        const CComPtr<IDiaSymbol> BaseClassSymbol = *It;
        const DWORD AccessModifier = GET_SYMBOL_ATTRIBUTE_CHECKED( BaseClassSymbol, access );
        const BOOL bVirtualBaseClass = GET_SYMBOL_ATTRIBUTE_CHECKED( BaseClassSymbol, virtualBaseClass );
        assert( !TypeNameInfo.bIsAnonymousSymbol && L"Anonymous symbols cannot have base classes" );

        const CComPtr<IDiaSymbol> BaseClassTypeSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( BaseClassSymbol, type );

        UDTDeclarationBaseClass BaseClassData{};
        BaseClassData.AccessModifier = CodeGeneration::ConvertAccessModifier( AccessModifier );
        BaseClassData.bIsVirtual = bVirtualBaseClass;
        BaseClassData.BaseClass = CodeGeneration::FormatTypeName( BaseClassTypeSymbol, TypeProvider );

        TypeDecl->BaseClasses.push_back( BaseClassData );
    }

    // Only generate nested types if we actually have a valid type info. We can have type generated without it if it is an anonymous type declaration
    if ( TypeInfo != nullptr )
    {
        // Generate nested enums
        std::unordered_map<DWORD, IUDTDeclarationMember*> NestedTypeMembersLookup;
        for ( const EnumerationInfo* NestedEnumInfo : TypeInfo->GetNestedEnums() )
        {
            const CComPtr<IDiaSymbol> EnumSymbol = NestedEnumInfo->GetEnumSymbol();
            const DWORD UniqueSymbolIndex = GET_SYMBOL_ATTRIBUTE_CHECKED( EnumSymbol, symIndexId );

            std::wstring NestedTypeNamespace;
            const std::shared_ptr<UDTNestedEnumDeclaration> NestedEnumDecl = std::make_shared<UDTNestedEnumDeclaration>( MakeEnum( EnumSymbol, &NestedTypeNamespace, TypeProvider ) );
            NestedEnumDecl->AccessModifier = CppAccessModifier::Public;
            NestedEnumDecl->Priority = 100;
            assert( NestedTypeNamespace == TypeNameInfo.OriginalFullName && L"Nested type is not located in the scope of the enclosing type!" );
            NestedTypeMembersLookup.insert({ UniqueSymbolIndex, NestedEnumDecl.get() });
            TypeDecl->Members.push_back( NestedEnumDecl );
        }

        // Generate nested types
        for ( const UserDefinedTypeInfo* NestedTypeInfo : TypeInfo->GetNestedTypes() )
        {
            const CComPtr<IDiaSymbol> NestedTypeSymbol = NestedTypeInfo->GetUDTSymbol();
            const DWORD UniqueSymbolIndex = GET_SYMBOL_ATTRIBUTE_CHECKED( NestedTypeSymbol, symIndexId );

            std::wstring NestedTypeNamespace;
            const std::shared_ptr<UDTNestedTypeDeclaration> NestedTypeDecl = std::make_shared<UDTNestedTypeDeclaration>( MakeUDT( NestedTypeInfo->GetUDTSymbol(), NestedTypeInfo, &NestedTypeNamespace, TypeProvider ) );
            NestedTypeDecl->AccessModifier = CppAccessModifier::Public;
            NestedTypeDecl->Priority = 100;
            assert( NestedTypeNamespace == TypeNameInfo.OriginalFullName && L"Nested type is not located in the scope of the enclosing type!" );
            NestedTypeMembersLookup.insert({ UniqueSymbolIndex, NestedTypeDecl.get() });
            TypeDecl->Members.push_back( NestedTypeDecl );
        }

        // Potentially generate dependencies between nested types
        NestedTypeDependencyCollector DependencyCollector( TypeProvider );
        DependencyCollector.PopulateWithNestedDeclarations( NestedTypeMembersLookup );

        for ( const UserDefinedTypeInfo* NestedTypeInfo : TypeInfo->GetNestedTypes() )
        {
            DependencyCollector.CollectDependenciesForNestedType( NestedTypeInfo );
        }
    }

    int32_t NumExternalDataMembers = 0;
    // Generate member functions
    bool bWantsDefaultMemberInitialization = false;
    int32_t NumConstructorsGenerated = 0;
    for ( DiaChildSymbolIterator It( UDTSymbol, SymTagFunction ); It; ++It )
    {
        const CComPtr<IDiaSymbol> FunctionSymbol = *It;
        const SymbolNameInfo FunctionNameInfo = SymbolNameInfo::FromSymbol( FunctionSymbol );

        // Skip lambda symbols
        if ( FunctionNameInfo.bIsLambdaSymbol ) continue;
        // Skip MSVC auto-generated internal functions such as __autoclassinit and __vecDelDtor
        if ( CodeGeneration::IsInternalSymbolName( FunctionNameInfo ) ) continue;

        // Check if we are generating a constructor or destructor
        const bool bIsConstructor = FunctionNameInfo.LocalName == TypeNameInfo.LocalName;
        const bool bIsConstructorOrDestructor = FunctionNameInfo.LocalName.starts_with(L'~') || bIsConstructor;
        const bool bIsAssignmentOperator = FunctionNameInfo.LocalName == L"operator=";

        // Anonymous types cannot have constructors or destructors declared, and neither they can have assignment operator defined - these are all compiler-generated
        if ( ( bIsConstructorOrDestructor || bIsAssignmentOperator ) && TypeNameInfo.bIsAnonymousSymbol ) continue;

        const DWORD AccessModifier = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionSymbol, access );
        const BOOL bIsFunctionVirtual = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSymbol, virtual );
        const BOOL bIsFunctionPureVirtual = bIsFunctionVirtual && GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSymbol, pure );
        const CComPtr<IDiaSymbol> FunctionSignatureSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionSymbol, type );
        const CComPtr<IDiaSymbol> ObjectPointerType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSignatureSymbol, objectPointerType );
        const CComPtr<IDiaSymbol> FunctionReturnType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSignatureSymbol, type );

        const std::shared_ptr<UDTFunctionDeclaration> FunctionDecl = std::make_shared<UDTFunctionDeclaration>();
        FunctionDecl->AccessModifier = CodeGeneration::ConvertAccessModifier( AccessModifier );
        FunctionDecl->MemberName = FunctionNameInfo.LocalName;

        if ( FunctionNameInfo.bIsTemplateInstantiation )
        {
            FunctionDecl->bIsTemplateSpecialization = true;
            const bool bTemplateArgumentsParsed = TypeTemplateArgument::ParseTemplateArguments( FunctionNameInfo.TemplateArguments, FunctionDecl->TemplateArguments );
            assert( bTemplateArgumentsParsed && L"Failed to parse template instantiation arguments" );
        }
        CodeGeneration::FormatFunctionType( FunctionSignatureSymbol, TypeProvider, FunctionDecl->ReturnType, FunctionDecl->ParameterNamesAndTypes, FunctionDecl->bIsVariadicArguments, nullptr, FunctionSymbol, &FunctionDecl->bIsConst );

        // Skip copy and move constructors in case they look generated (generated ones will have argument named __that)
        // TODO: This rule might need more refining to work correctly outside of MSVC compiled binaries, but this will do for now.
        if ( bIsConstructor && FunctionDecl->ParameterNamesAndTypes.size() == 1 && FunctionDecl->ParameterNamesAndTypes[0].first == L"__that" )
        {
            continue;
        }

        FunctionDecl->bIsStatic = ObjectPointerType == nullptr; // we are static if we have no object pointer argument
        FunctionDecl->bIsVirtual = bIsFunctionVirtual;
        FunctionDecl->bIsPureVirtual = bIsFunctionPureVirtual;
        if ( bIsFunctionVirtual && !bIsConstructorOrDestructor )
        {
            assert( !FunctionNameInfo.bIsTemplateInstantiation && L"templated virtual functions are definitely not a sane thing to support" );

            if ( const CComPtr<IDiaSymbol> ParentClassFunction = DiaUtils::FindParentVirtualFunction( UDTSymbol, FunctionNameInfo.LocalName, FunctionSignatureSymbol ) )
            {
                FunctionDecl->bIsOverride = true;
                // Skip the function generation altogether if this is the same implementation as in the parent function
                if ( DiaUtils::IsFunctionImplementationTheSame( ParentClassFunction, FunctionSymbol ) ) continue;
            }
        }

        // TODO: There are also type conversion operators that should not have return types either
        // TODO: Cases where constructors can be templated when their owner class is not a template exist, this will most likely need more testing and adjusting based on the DLL in question
        // Destructors and constructors do not have a return type, and generally should not be template instantiations
        if ( bIsConstructorOrDestructor )
        {
            FunctionDecl->MemberName = FunctionNameInfo.ToString( SymbolNameInfo::IncludeLocalNameOnly );
            FunctionDecl->bNoReturnType = true;
            FunctionDecl->TemplateArguments.clear();
            FunctionDecl->bIsTemplateSpecialization = false;
        }
        // Mark constructors with the single argument explicit
        if ( bIsConstructor && FunctionDecl->ParameterNamesAndTypes.size() == 1 )
        {
            FunctionDecl->bIsExplicit = true;
        }

        DWORD SymbolLocationType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSymbol, locationType );
        assert( SymbolLocationType == LocIsStatic || SymbolLocationType == LocIsNull );

        // Treat symbols with static location that are not public as symbols with no location
        if ( SymbolLocationType == LocIsStatic )
        {
            const DWORD SymbolRelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionSymbol, relativeVirtualAddress );
            if ( TypeProvider->FindPublicSymbolByRVA( SymbolRelativeVirtualAddress ) == nullptr )
            {
                SymbolLocationType = LocIsNull;
            }
        }

        // Treat function symbols that do not actually exist in the executable. These cannot really be called and should not be exported
        if ( SymbolLocationType == LocIsNull )
        {
            // We can silently discard functions that do not have an implementation if they are not virtual
            if ( bIsFunctionVirtual )
            {
                // If this is a definition of the virtual function that does not have a body, it is implicitly pure virtual.
                // TODO: I don't know why DIA SDK does not correctly mark pure virtual functions as pure, and instead uses LocIsNull as location.
                if ( !FunctionDecl->bIsOverride )
                {
                    FunctionDecl->bIsPureVirtual = true;
                }
                else
                {
                    // Otherwise, it is a virtual function that got duplicate code eliminated and does not have a code
                    // There are many solutions to this, most likely the code for this function is still in the executable, but DIA SDK does not give a good way
                    // to find it, and also does not give a good way to check the mangled name for this function, so right now we just define the function as assert(0)
                    // TODO: Look into this more. I am NOT GONNA implement the entire fucking MSVC name mangler just to resolve this edge case
                    std::wstring SynthetisedReturnValue;
                    if ( FunctionReturnType && CodeGeneration::GenerateDefaultValueForSimpleType( FunctionReturnType, SynthetisedReturnValue ) )
                    {
                        FunctionDecl->InlineImplementation = StringPrintf(L"{ __debugbreak(); __assume(0); return %s; }", SynthetisedReturnValue.c_str());
                    }
                    else
                    {
                        FunctionDecl->InlineImplementation = StringPrintf(L"{ __debugbreak(); __assume(0); }");
                    }
                }
            }
            // We have to always emit the destructor and the default empty constructor if it is present though, these will be marked as forceinline instead of pure virtual
            else if ( bIsConstructor && FunctionDecl->ParameterNamesAndTypes.empty() )
            {
                FunctionDecl->Comment = L"Default constructor inlined, assuming default-initialization for members";
                FunctionDecl->InlineImplementation = L" = default;";
                bWantsDefaultMemberInitialization = true;
            }
            else if ( bIsConstructorOrDestructor && FunctionDecl->ParameterNamesAndTypes.empty() )
            {
                FunctionDecl->Comment = L"Destructor unavailable, assuming default implementation";
                FunctionDecl->InlineImplementation = L" = default;";
            }
            else
            {
                // Otherwise, we can skip the member
                continue;
            }
        }
        else
        {
            // Only count actually generated constructors towards generated ones
            if ( bIsConstructor )
            {
                NumConstructorsGenerated++;
            }
            // Only non-null location symbols count as external
            NumExternalDataMembers++;
        }
        TypeDecl->Members.push_back( FunctionDecl );
    }
    // We want member initialization if we generated no constructors whatsoever
    bWantsDefaultMemberInitialization |= NumConstructorsGenerated == 0;

    // Generate member variables
    for ( DiaChildSymbolIterator It( UDTSymbol, SymTagData ); It; ++It )
    {
        const CComPtr<IDiaSymbol> DataSymbol = *It;
        const SymbolNameInfo DataNameInfo = SymbolNameInfo::FromSymbol( DataSymbol );

        // Skip lambda symbols
        if ( DataNameInfo.bIsLambdaSymbol ) continue;
        // Skip MSVC auto-generated internal functions such as __autoclassinit and __vecDelDtor
        if ( CodeGeneration::IsInternalSymbolName( DataNameInfo ) ) continue;

        const DWORD DataLocationType = GET_SYMBOL_ATTRIBUTE_CHECKED( DataSymbol, locationType );
        const DWORD AccessModifier = GET_SYMBOL_ATTRIBUTE_CHECKED( DataSymbol, access );
        const BOOL bIsDataConst = GET_SYMBOL_ATTRIBUTE_CHECKED( DataSymbol, constType );
        const CComPtr<IDiaSymbol> DataTypeSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( DataSymbol, type );

        assert( DataLocationType == LocIsStatic || DataLocationType == LocIsThisRel || DataLocationType == LocIsBitField || DataLocationType == LocIsConstant || DataLocationType == LocIsNull || DataLocationType == LocIsTLS );

        // Skip members that do not have the location in the executable
        if (DataLocationType == LocIsNull) continue;

        // Skip static data members that are not public symbols since they are not visible to the outside code
        if ( DataLocationType == LocIsStatic )
        {
            const DWORD SymbolRelativeVirtualAddress = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( DataSymbol, relativeVirtualAddress );
            if ( SymbolRelativeVirtualAddress == 0 )
            {
                std::wcerr << L"Skipping class data member with static location but no valid virtual address: " << DataNameInfo.ToString() << std::endl;
                continue;
            }
            if ( TypeProvider->FindPublicSymbolByRVA( SymbolRelativeVirtualAddress ) == nullptr )
            {
                continue;
            }
        }
        const std::shared_ptr<UDTDataMemberDeclaration> DataDecl = std::make_shared<UDTDataMemberDeclaration>();

        DataDecl->MemberName = DataNameInfo.LocalName;
        DataDecl->MemberType = CodeGeneration::FormatTypeName( DataTypeSymbol, TypeProvider );
        DataDecl->AccessModifier = CodeGeneration::ConvertAccessModifier( AccessModifier );
        DataDecl->bIsConst = bIsDataConst;
        DataDecl->bIsStatic = DataLocationType == LocIsStatic || DataLocationType == LocIsConstant || DataLocationType == LocIsTLS;
        DataDecl->bIsThreadLocal = DataLocationType == LocIsTLS;

        // Emit default initialization in case we emitted a no-op default constructor
        // We only need to do that for basic types and pointers, since they do not have default initializers.
        if ( DataLocationType == LocIsThisRel && bWantsDefaultMemberInitialization )
        {
            if ( DataDecl->MemberType->GetId() == ETypeDeclarationId::FundamentalType || DataDecl->MemberType->GetId() == ETypeDeclarationId::PointerType )
            {
                DataDecl->bWantsDefaultInitializer = true;
            }
        }

        if ( DataNameInfo.bIsTemplateInstantiation )
        {
            assert( DataLocationType == LocIsStatic && L"Only static member variables can be templated" );
            DataDecl->bIsTemplateSpecialization = true;
            const bool bTemplateArgumentsParsed = TypeTemplateArgument::ParseTemplateArguments( DataNameInfo.TemplateArguments, DataDecl->TemplateArguments );
            assert( bTemplateArgumentsParsed && L"Failed to parse template instantiation arguments" );
        }
        // Bitfields have their length in bits and not bytes. They also have bit offset, but we cannot set it directly
        if ( DataLocationType == LocIsBitField )
        {
            const ULONGLONG BitfieldSizeBits = GET_SYMBOL_ATTRIBUTE_CHECKED( DataSymbol, length );
            DataDecl->BitfieldSize = static_cast<int32_t>(BitfieldSizeBits);
        }
        // Generate constant value
        if ( DataLocationType == LocIsConstant )
        {
            const CComVariant Variant = GET_SYMBOL_ATTRIBUTE_CHECKED( DataSymbol, value );
            DataDecl->ConstantValue = CodeGeneration::GenerateConstantValue( Variant );
            DataDecl->bIsConstexpr = true;
        }
        TypeDecl->Members.push_back( DataDecl );

        // Only static data members count as external
        if ( DataLocationType == LocIsStatic )
        {
            NumExternalDataMembers++;
        }
    }

    // Import the type if it has any static data members or any kind of a function or if it is a class
    TypeDecl->bIsDllImport = NumExternalDataMembers > 0 || TypeDecl->Kind == CppUDTKind::Class;

    return TypeDecl;
}

std::shared_ptr<ITopLevelDeclaration> GeneratedHeaderFile::MakePredeclaration(const CComPtr<IDiaSymbol>& InEnumSymbol, ITypeResolutionProvider* TypeProvider)
{
    return std::make_shared<PredeclarationStatement>( CodeGeneration::FormatTypeName( InEnumSymbol, TypeProvider ) );
}

std::shared_ptr<ITopLevelDeclaration> GeneratedHeaderFile::MakeTemplateDeclaration( const SymbolNameInfo& TemplateInstantiationName, const CComPtr<IDiaSymbol>& TemplateInstantiationSymbol, ITypeResolutionProvider* TypeProvider)
{
    const DWORD UDTKind = GET_SYMBOL_ATTRIBUTE_CHECKED( TemplateInstantiationSymbol, udtKind );

    const std::shared_ptr<TemplateTypeDeclaration> Declaration = std::make_shared<TemplateTypeDeclaration>();
    Declaration->UDTKind = CodeGeneration::ConvertUDTKind( UDTKind );
    Declaration->Namespace = TemplateInstantiationName.SymbolScope;
    Declaration->ClassName = TemplateInstantiationName.LocalName;

    std::vector<TypeTemplateArgument> TemplateArguments;
    const bool bParsedTemplateArguments = TypeTemplateArgument::ParseTemplateArguments( TemplateInstantiationName.TemplateArguments, TemplateArguments );
    assert( bParsedTemplateArguments && L"Failed to parse template arguments to emit a template declaration" );

    for ( const TypeTemplateArgument& Argument : TemplateArguments )
    {
        TemplateDeclarationArgument DeclarationArgument;
        if ( Argument.Type == ETemplateArgumentType::TypeDeclaration )
        {
            DeclarationArgument.Type = ETemplateDeclarationArgumentType::Typename;
        }
        else if ( Argument.Type == ETemplateArgumentType::TypeMemberReference )
        {
            DeclarationArgument.Type = ETemplateDeclarationArgumentType::TypeValue;

            // TODO: We do not have enough information to infer the type of the underlying template. void* will work on MSVC, since function pointers can be implicitly casted to void*,
            // but will most likely not work on other platforms for member function pointer types and member pointer types
            const std::shared_ptr<PointerTypeDeclaration> TypeKind = std::make_shared<PointerTypeDeclaration>();
            TypeKind->PointeeType = std::make_shared<VoidTypeDeclaration>();
            DeclarationArgument.TypeValueKind = TypeKind;
        }
        else if ( Argument.Type == ETemplateArgumentType::IntegerConst )
        {
            DeclarationArgument.Type = ETemplateDeclarationArgumentType::TypeValue;
            const std::shared_ptr<FundamentalTypeDeclaration> TypeKind = std::make_shared<FundamentalTypeDeclaration>();
            TypeKind->BasicType = EBasicType::Int;
            TypeKind->bIsLongLong = true;
            DeclarationArgument.TypeValueKind = TypeKind;
        }
        else
        {
            assert(!L"Unsupported type template argument type");
        }
        Declaration->Arguments.push_back(DeclarationArgument);
    }
    return Declaration;
}

HeaderFileReferenceCollector::HeaderFileReferenceCollector(const GeneratedHeaderFile* InOwnerHeader) : TypeDependencyCollectorBase( InOwnerHeader->GetHeaderGenerator() ), OwnerHeader( InOwnerHeader )
{
}

void HeaderFileReferenceCollector::PopulateWithTopLevelDefinitions(const std::unordered_map<IDiaSymbol*, ITopLevelDeclaration*>& InRawDefinitionMap)
{
    // Associate each symbol with the unique ID in the map
    for ( const auto& [RawSymbol, Declaration] : InRawDefinitionMap )
    {
        const DWORD SymbolUniqueId = GET_SYMBOL_ATTRIBUTE_CHECKED( RawSymbol, symIndexId );
        TopLevelDeclarations.insert({ SymbolUniqueId, Declaration });
    }

    // Collect dependencies for each single one of the symbols
    for ( IDiaSymbol* TopLevelSymbol : InRawDefinitionMap | std::ranges::views::keys )
    {
        CurrentlyCollectingDeclaration = InRawDefinitionMap.find( TopLevelSymbol )->second;
        // Clear the dependencies from the previous symbol we collected
        DependenciesAlreadyHandled.clear();

        const DWORD SymbolTagId = GET_SYMBOL_ATTRIBUTE_CHECKED( TopLevelSymbol, symTag );
        if ( SymbolTagId == SymTagUDT )
        {
            const UserDefinedTypeInfo* UserDefinedType = OwnerHeader->GetHeaderGenerator()->FindUserDefinedType( TopLevelSymbol );
            assert( UserDefinedType != nullptr );
            // Collect dependencies for this type and all of the nested types
            CollectDependenciesForTypeAndNestedTypes( UserDefinedType );

            // If this is a template instantiation, we need to emit a template declaration for it and add a dependency on it
            // TODO: Also need to do this for nested template types.
            const SymbolNameInfo TypeSymbolName = SymbolNameInfo::FromSymbol( TopLevelSymbol );
            if ( TypeSymbolName.bIsTemplateInstantiation )
            {
                ITopLevelDeclaration* TemplateDeclaration = FindOrCreateTemplateDeclaration( TypeSymbolName, TopLevelSymbol );
                CurrentlyCollectingDeclaration->Dependencies.insert(TemplateDeclaration);
            }
        }
        else if ( SymbolTagId == SymTagEnum )
        {
            const CComPtr<IDiaSymbol> UnderlyingType = GET_SYMBOL_ATTRIBUTE_CHECKED( TopLevelSymbol, type );
            CollectDependenciesForType( UnderlyingType );
        }
        else if ( SymbolTagId == SymTagFunction || SymbolTagId == SymTagData )
        {
            const CComPtr<IDiaSymbol> FunctionOrDataType = GET_SYMBOL_ATTRIBUTE_CHECKED( TopLevelSymbol, type );
            CollectDependenciesForType( FunctionOrDataType );
        }
        else
        {
            assert(!L"Unhandled top level generated symbol type, only functions, data, UDTs and enums are expected");
        }
    }

    CurrentlyCollectingDeclaration = nullptr;
    DependenciesAlreadyHandled.clear();

    // Append the dependency on the <cstdint> if needed
    if ( NeedsCStdint() )
    {
        SystemHeaderIncludes.insert(L"cstdint");
    }
}

void HeaderFileReferenceCollector::CollectDependenciesForTypeAndNestedTypes(const UserDefinedTypeInfo* InTypeInfo )
{
    CollectDependenciesForUDTDefinition( InTypeInfo->GetUDTSymbol() );

    // If this is a template instantiation, parse arguments and collect dependencies for all of it's template arguments
    const SymbolNameInfo& TypeNameInfo = InTypeInfo->GetSymbolName();
    if ( TypeNameInfo.bIsTemplateInstantiation )
    {
        TypeTemplateArgumentContainer TypeTemplateArguments;
        const bool bParsedTemplateArguments = TypeTemplateArgumentContainer::ParseTemplateArguments( TypeNameInfo.TemplateArguments, TypeTemplateArguments );
        assert( bParsedTemplateArguments && L"Failed to parse template instantiation arguments for type generated inside of the header file" );

        CollectDependenciesForTemplateInstantiation( TypeNameInfo, TypeTemplateArguments );
    }

    for ( const UserDefinedTypeInfo* NestedType : InTypeInfo->GetNestedTypes() )
    {
        CollectDependenciesForTypeAndNestedTypes( NestedType );
    }
    for ( const EnumerationInfo* NestedEnum : InTypeInfo->GetNestedEnums() )
    {
        CollectDependenciesForType( NestedEnum->GetEnumSymbol() );
    }
}

void HeaderFileReferenceCollector::PopulateGeneratedFileWithDependencies(CppFile& CppFile) const
{
    CppFile.LocalIncludes = std::vector( LocalHeaderIncludes.begin(), LocalHeaderIncludes.end() );
    CppFile.SystemIncludes = std::vector( SystemHeaderIncludes.begin(), SystemHeaderIncludes.end() );
    CppFile.Declarations.insert( CppFile.Declarations.end(), AllPredeclarations.begin(), AllPredeclarations.end() );
}

void HeaderFileReferenceCollector::HandleTypedefDependency(const std::wstring& TypedefName, const CComPtr<IDiaSymbol>& TypedefType)
{
    assert( !L"Typedef dependency collection is not supported yet" );
}

bool HeaderFileReferenceCollector::HandleTemplateInstantiationDependency(const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments)
{
    // Check if this is a symbol that we consider special or external. If it is, lookup the external file
    if ( OwnerHeader->GetHeaderGenerator()->ShouldMarkSymbolAsExternal( TemplateName, EExternalSymbolType::Type ) )
    {
        // Attempt to find an external header for this type. If we found it, continue.
        if ( const std::wstring* ExternalFilename = OwnerHeader->GetHeaderGenerator()->FindExternalHeaderForType( TemplateName ) )
        {
            SystemHeaderIncludes.insert( *ExternalFilename );
            return true;
        }
        std::wcerr << L"Failed to find header for external template instantiation. It will not be correctly included into the header file: " << TemplateName.ToString( SymbolNameInfo::IncludeNamespace ) << std::endl;
        return true;
    }
    // Check if this is a template coming from the overriden header that is not considered external, but is not generated by us either.
    if ( const std::wstring* HeaderOverridenFilename = OwnerHeader->GetHeaderGenerator()->FindOverridenManualHeaderForType( TemplateName ) )
    {
        LocalHeaderIncludes.insert( *HeaderOverridenFilename );
        return true;
    }
    return false;
}

ITopLevelDeclaration* HeaderFileReferenceCollector::FindOrCreateTemplateDeclaration( const SymbolNameInfo& TemplateInstantiationName, const CComPtr<IDiaSymbol>& TemplateInstantiationSymbol )
{
    const std::wstring TemplateFullName = TemplateInstantiationName.ToString( SymbolNameInfo::IncludeNamespace );

    // Check if there is an existing predeclaration for this template
    if ( const auto ExistingPredeclaration = TemplateToPredeclarationLookup.find( TemplateFullName ); ExistingPredeclaration != TemplateToPredeclarationLookup.end() )
    {
        return ExistingPredeclaration->second;
    }

    const std::shared_ptr<ITopLevelDeclaration> NewTemplateDeclaration = GeneratedHeaderFile::MakeTemplateDeclaration( TemplateInstantiationName, TemplateInstantiationSymbol, OwnerHeader->GetHeaderGenerator() );
    AllPredeclarations.push_back( NewTemplateDeclaration );
    TemplateToPredeclarationLookup.insert({ TemplateFullName, NewTemplateDeclaration.get() });
    return NewTemplateDeclaration.get();
}

void HeaderFileReferenceCollector::AddDependencyInternal(const TypeDependency& TypeDependency, bool bIsPredeclaration)
{
    assert( CurrentlyCollectingDeclaration );

    // Find top level type that this dependency represents
    const CComPtr<IDiaSymbol> TopLevelDependency = TypeResolver->FindTopLevelType( TypeDependency.GetSymbol() );
    const DWORD DependencyUniqueId = GET_SYMBOL_ATTRIBUTE_CHECKED( TopLevelDependency, symIndexId );
    const DWORD DependencySymTag = GET_SYMBOL_ATTRIBUTE_CHECKED( TopLevelDependency, symTag );

    // Handle pre-declarations first. We do not need to worry about this not being a top level type because reference collector will never emit pre-declarations for nested types
    if ( bIsPredeclaration )
    {
        // If we already have a predeclaration for this type, just add ourselves as a dependecy for it
        if ( const auto ExistingPredeclaration = TypeToPredeclarationLookup.find( DependencyUniqueId ); ExistingPredeclaration != TypeToPredeclarationLookup.end() )
        {
            CurrentlyCollectingDeclaration->Dependencies.insert( ExistingPredeclaration->second );
            return;
        }

        // Handle template predeclarations. All template instantiations only need one shared pre-declaration
        const SymbolNameInfo SymbolName = SymbolNameInfo::FromSymbol( TypeDependency.GetSymbol() );

        // Do not attempt to pre-declare C++ standard library types and external types, always include them instead
        if ( OwnerHeader->GetHeaderGenerator()->ShouldMarkSymbolAsExternal( SymbolName, EExternalSymbolType::Type ) ||
            OwnerHeader->GetHeaderGenerator()->IsExternalUserDefinedType( TypeDependency.GetSymbol() ) ||
            CodeGeneration::IsInternalSymbolName( SymbolName ) )
        {
            AddDependencyInternal( TypeDependency, false );
            return;
        }

        // Attempt to handle the template instantiation pre-declaration first
        if ( SymbolName.bIsTemplateInstantiation )
        {
            ITopLevelDeclaration* TemplateDeclaration = FindOrCreateTemplateDeclaration( SymbolName, TypeDependency.GetSymbol() );

            // On top of the bare template declaration, we might need to include the arguments for the template instantiation
            // Parse them and then attempt to resolve the UDTs matching these arguments
            TypeTemplateArgumentContainer TemplateArguments;
            const bool bParsedTemplateArguments = TypeTemplateArgumentContainer::ParseTemplateArguments( SymbolName.TemplateArguments, TemplateArguments );
            assert( bParsedTemplateArguments && L"Failed to parse template arguments for dependency collection" );

            CollectDependenciesForTemplateInstantiation( SymbolName, TemplateArguments );
            TypeToPredeclarationLookup.insert({ DependencyUniqueId, TemplateDeclaration });
            CurrentlyCollectingDeclaration->Dependencies.insert( TemplateDeclaration );
            return;
        }

        // Otherwise, we need to generate a new predeclaration
        const std::shared_ptr<ITopLevelDeclaration> NewPredeclaration = GeneratedHeaderFile::MakePredeclaration( TypeDependency.GetSymbol(), OwnerHeader->GetHeaderGenerator() );

        // Register the new predeclaration if we managed to produce it, otherwise, we need to include the full definition.
        if ( NewPredeclaration )
        {
            AllPredeclarations.push_back( NewPredeclaration );
            TypeToPredeclarationLookup.insert({ DependencyUniqueId, NewPredeclaration.get() });
            CurrentlyCollectingDeclaration->Dependencies.insert( NewPredeclaration.get() );
            return;
        }
    }

    if ( DependenciesAlreadyHandled.contains( DependencyUniqueId ) ) return;
    DependenciesAlreadyHandled.insert( DependencyUniqueId );

    // If this is the dependency on one of the types or enums we have declared in this file, look it up
    if ( const auto HeaderLocalDeclaration = TopLevelDeclarations.find( DependencyUniqueId ); HeaderLocalDeclaration != TopLevelDeclarations.end() )
    {
        // Do not produce dependencies on ourselves
        if ( HeaderLocalDeclaration->second == CurrentlyCollectingDeclaration )
        {
            return;
        }
        CurrentlyCollectingDeclaration->Dependencies.insert( HeaderLocalDeclaration->second );
    }
    // Attempt to look up the header file associated with the dependency next
    else if ( const GeneratedHeaderFile* AssociatedHeaderFile = OwnerHeader->GetHeaderGenerator()->FindHeaderFileForSymbol( TopLevelDependency ) )
    {
        // We should never end up with looking up for ouselves here
        assert( AssociatedHeaderFile != OwnerHeader );
        // Add the dependency on the local header
        LocalHeaderIncludes.insert( AssociatedHeaderFile->GetHeaderFileName() );
    }
    else
    {
        const SymbolNameInfo TopLevelSymbolName = SymbolNameInfo::FromSymbol( TopLevelDependency );

        // If this is an external dependency on the template instantiation, we need to also include dependencies for that template's arguments
        if ( TopLevelSymbolName.bIsTemplateInstantiation )
        {
            TypeTemplateArgumentContainer TemplateArguments;
            const bool bParsedTemplateArguments = TypeTemplateArgumentContainer::ParseTemplateArguments( TopLevelSymbolName.TemplateArguments, TemplateArguments );
            assert( bParsedTemplateArguments && L"Failed to parse template arguments for dependency collection" );

            CollectDependenciesForTemplateInstantiation( TopLevelSymbolName, TemplateArguments );
        }

        // Check if this is an unnamed symbol. If this is an unnamed symbol, it is declared locally, and we do not need to include anything for it, but we need to collect it's dependencies
        if ( TopLevelSymbolName.bIsAnonymousSymbol || TopLevelSymbolName.bIsLambdaSymbol )
        {
            // Only need to collect dependencies recursively for UDT dependencies
            if ( DependencySymTag == SymTagUDT )
            {
                CollectDependenciesForUDTDefinition( TopLevelDependency, false );
            }
        }
        // Attempt to lookup external dependency
        else if ( const std::wstring* ExternalDependencyName = OwnerHeader->GetHeaderGenerator()->FindExternalHeaderForType( TopLevelSymbolName ) )
        {
            SystemHeaderIncludes.insert( *ExternalDependencyName );
        }
        // Check if this is an overriden header manually provided by the user
        else if ( const std::wstring* ManualOverridenHeaderName = OwnerHeader->GetHeaderGenerator()->FindOverridenManualHeaderForType( TopLevelSymbolName ) )
        {
            LocalHeaderIncludes.insert( *ManualOverridenHeaderName );
        }
        // Print the missing type, assuming it is not an anonymous type that we have already inlined into the definition, but still gathered as a dependency
        else if ( !TopLevelSymbolName.bIsAnonymousSymbol )
        {
            const std::wstring TypeName = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( TopLevelDependency, name );
            std::wcerr << L"Unhandled dependency on the external type/enum: " << TypeName << L". Make sure the type is defined inside of the external libraries and associated with a header via ExternalTypeMappings.json" << std::endl;
        }
    }
}

NestedTypeDependencyCollector::NestedTypeDependencyCollector( ITypeResolutionProvider* InTypeProvider ) : TypeDependencyCollectorBase( InTypeProvider )
{
}

void NestedTypeDependencyCollector::PopulateWithNestedDeclarations(const std::unordered_map<DWORD, IUDTDeclarationMember*>& InNestedTypeDeclarations)
{
    NestedTypeDeclarations = InNestedTypeDeclarations;
}

void NestedTypeDependencyCollector::CollectDependenciesForNestedType(const UserDefinedTypeInfo* InTypeInfo)
{
    const CComPtr<IDiaSymbol> TypeSymbol = InTypeInfo->GetUDTSymbol();
    const DWORD TypeSymbolIndex = GET_SYMBOL_ATTRIBUTE_CHECKED( TypeSymbol, symIndexId );

    assert( NestedTypeDeclarations.contains( TypeSymbolIndex ) );

    CurrentlyCollectingDeclaration = NestedTypeDeclarations.find( TypeSymbolIndex )->second;
    CollectDependenciesForTypeAndNestedTypes( InTypeInfo );
    CurrentlyCollectingDeclaration = nullptr;
    DependenciesAlreadyHandled.clear();
}

void NestedTypeDependencyCollector::CollectDependenciesForTypeAndNestedTypes(const UserDefinedTypeInfo* InTypeInfo)
{
    CollectDependenciesForUDTDefinition( InTypeInfo->GetUDTSymbol() );

    // If this is a template instantiation, parse arguments and collect dependencies for all of it's template arguments
    if (const SymbolNameInfo& TypeNameInfo = InTypeInfo->GetSymbolName(); TypeNameInfo.bIsTemplateInstantiation )
    {
        TypeTemplateArgumentContainer TypeTemplateArguments;
        const bool bParsedTemplateArguments = TypeTemplateArgumentContainer::ParseTemplateArguments( TypeNameInfo.TemplateArguments, TypeTemplateArguments );
        assert( bParsedTemplateArguments && L"Failed to parse template instantiation arguments for type generated inside of the header file" );

        CollectDependenciesForTemplateInstantiation( TypeNameInfo, TypeTemplateArguments );
    }

    for ( const UserDefinedTypeInfo* NestedType : InTypeInfo->GetNestedTypes() )
    {
        CollectDependenciesForTypeAndNestedTypes( NestedType );
    }
    for ( const EnumerationInfo* NestedEnum : InTypeInfo->GetNestedEnums() )
    {
        CollectDependenciesForType( NestedEnum->GetEnumSymbol() );
    }
}

bool NestedTypeDependencyCollector::HandleTemplateInstantiationDependency(const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments)
{
    // We should never need to rely on a manual dependency lookup, since we always have all template instantiations available to us
    // TODO: Implement matching against local template instantiations on the same level, but since nested template instantiations depending on each other is an extremely rare case, this can be implemented later
    return true;
}

void NestedTypeDependencyCollector::AddDependencyInternal(const TypeDependency& TypeDependency, bool bIsPredeclaration)
{
    assert( CurrentlyCollectingDeclaration );

    const DWORD DependencyUniqueId = GET_SYMBOL_ATTRIBUTE_CHECKED( TypeDependency.GetSymbol(), symIndexId );
    if ( DependenciesAlreadyHandled.contains( DependencyUniqueId ) ) return;
    DependenciesAlreadyHandled.insert( DependencyUniqueId );

    const SymbolNameInfo SymbolName = SymbolNameInfo::FromSymbol( TypeDependency.GetSymbol() );

    // If this is a template instantiation, make sure to collect it's dependencies, since they might be other sub-types that we want to depend on the same level
    if ( SymbolName.bIsTemplateInstantiation )
    {
        // On top of the bare template declaration, we might need to include the arguments for the template instantiation
        // Parse them and then attempt to resolve the UDTs matching these arguments
        TypeTemplateArgumentContainer TemplateArguments;
        const bool bParsedTemplateArguments = TypeTemplateArgumentContainer::ParseTemplateArguments( SymbolName.TemplateArguments, TemplateArguments );
        assert( bParsedTemplateArguments && L"Failed to parse template arguments for subtype dependency collection" );

        CollectDependenciesForTemplateInstantiation( SymbolName, TemplateArguments );
    }

    // We are only interested in the local dependencies on other members
    if ( const auto TypeLocalDeclaration = NestedTypeDeclarations.find( DependencyUniqueId ); TypeLocalDeclaration != NestedTypeDeclarations.end() )
    {
        // Do not produce dependencies on ourselves
        if ( TypeLocalDeclaration->second == CurrentlyCollectingDeclaration )
        {
            return;
        }
        CurrentlyCollectingDeclaration->Dependencies.insert( TypeLocalDeclaration->second );
    }
}
