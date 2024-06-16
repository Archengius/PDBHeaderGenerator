#include "CodeGeneration.h"
#include "GeneratedHeaderFile.h"
#include "AST/CppDeclarationTree.h"
#include "Utils/StringUtils.h"
#include <iostream>

CppAccessModifier CodeGeneration::ConvertAccessModifier(const DWORD InAccessModifier)
{
    switch ( InAccessModifier )
    {
        case CV_private: return CppAccessModifier::Private;
        case CV_protected: return CppAccessModifier::Protected;
        case CV_public: return CppAccessModifier::Public;
        default:;
    }
    assert(!L"unknown DIA SDK access modifier value");
    return CppAccessModifier::Public;
}

CppUDTKind CodeGeneration::ConvertUDTKind(const DWORD InUDTKind)
{
    switch ( InUDTKind )
    {
        case UdtClass: return CppUDTKind::Class;
        case UdtInterface: return CppUDTKind::Class;
        case UdtStruct: return CppUDTKind::Struct;
        case UdtUnion: return CppUDTKind::Union;
        case UdtTaggedUnion: return CppUDTKind::Union;
        default:;
    }
    assert(!L"unknown DIA SDK UDT kind");
    return CppUDTKind::Class;
}

static bool IsInternalSymbolNameInternal(const std::wstring& InFunctionName)
{
    // C++ runtime and standard library functions, starting with 2 underscores or an underscore and an uppercase letter
    if ( InFunctionName.size() >= 2 && InFunctionName[0] == L'_' && ( InFunctionName[1] == L'_' || iswupper( InFunctionName[1] ) ) )
    {
        return true;
    }
    // $initializer$ symbols are dynamic initializer function pointers, and symbols starting with tilde (`) are dynamic initializer arrays
    // TODO: $initializer$ is a valid function name, we could potentially be treating a normal user-defined function as special
    if( InFunctionName.find(L'`') != std::wstring::npos || InFunctionName.find(L"$initializer$") != std::wstring::npos )
    {
        return true;
    }
    // Should not generate headers for DllMain as well
    if ( InFunctionName == L"DllMain" )
    {
        return true;
    }
    return false;
}

bool CodeGeneration::IsInternalSymbolName(const SymbolNameInfo& SymbolName)
{
   return IsInternalSymbolNameInternal( SymbolName.LocalName ) ||
       IsInternalSymbolNameInternal( SymbolName.SymbolScope ) ||
       (SymbolName.SymbolScope == L"std" || SymbolName.SymbolScope.starts_with(L"std::"));
}

std::wstring EscapeString( std::wstring CharLiteral )
{
    CharLiteral = ReplaceAll( CharLiteral, L"\\", L"\\\\" );
    CharLiteral = ReplaceAll( CharLiteral, L"\"", L"\\\"" );
    CharLiteral = ReplaceAll( CharLiteral, L"\'", L"\\\'" );
    CharLiteral = ReplaceAll( CharLiteral, L"\?", L"\\\?" );
    CharLiteral = ReplaceAll( CharLiteral, L"\a", L"\\\a" );
    CharLiteral = ReplaceAll( CharLiteral, L"\b", L"\\\b" );
    CharLiteral = ReplaceAll( CharLiteral, L"\f", L"\\\f" );
    CharLiteral = ReplaceAll( CharLiteral, L"\n", L"\\\n" );
    CharLiteral = ReplaceAll( CharLiteral, L"\r", L"\\\r" );
    CharLiteral = ReplaceAll( CharLiteral, L"\t", L"\\\t" );
    CharLiteral = ReplaceAll( CharLiteral, L"\v", L"\\\v" );
    return CharLiteral;
}

std::wstring CodeGeneration::GenerateConstantValue(const VARIANT& InVariant)
{
    if ( InVariant.vt == VT_I1 || InVariant.vt == VT_I2 || InVariant.vt == VT_I4 || InVariant.vt == VT_I8 || InVariant.vt == VT_INT )
    {
        return StringPrintf(L"%lld", InVariant.vt == VT_I1 ? InVariant.cVal : InVariant.vt == VT_I2 ? InVariant.iVal : InVariant.vt == VT_I4 ? InVariant.lVal : InVariant.lVal == VT_I8 ? InVariant.llVal : InVariant.intVal );
    }
    if ( InVariant.vt == VT_UI1 || InVariant.vt == VT_UI2 || InVariant.vt == VT_UI4 || InVariant.vt == VT_UI8 || InVariant.vt == VT_UINT )
    {
        return StringPrintf(L"%llu", InVariant.vt == VT_UI1 ? InVariant.bVal : InVariant.vt == VT_UI2 ? InVariant.uiVal : InVariant.vt == VT_UI4 ? InVariant.ulVal : InVariant.lVal == VT_UI8 ? InVariant.ullVal : InVariant.uintVal );
    }
    if ( InVariant.vt == VT_R4 || InVariant.vt == VT_R8 )
    {
        return StringPrintf(L"%.2f", InVariant.vt == VT_R4 ? InVariant.fltVal : InVariant.dblVal );
    }
    if ( InVariant.vt == VT_BSTR )
    {
        // Replace quotes with escaped quotes, slashes with escaped slashes, and special characters with their equivalents
        const std::wstring CharLiteral = InVariant.bstrVal;
        return StringPrintf(L"\"%s\"", EscapeString( CharLiteral ).c_str());
    }
    assert( L"Unsupported VARIANT for constant value generation" );
    return L"<Unknown Variant>";
}

std::shared_ptr<ITypeDeclaration> CodeGeneration::FormatTypeName(const CComPtr<IDiaSymbol>& InTypeSymbol, ITypeResolutionProvider* TypeProvider )
{
    const DWORD SymbolTag = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, symTag );

    if ( SymbolTag == SymTagArrayType )
    {
        const CComPtr<IDiaSymbol> ElementType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, type );
        const DWORD ArrayCount = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, count );

        const std::shared_ptr<ITypeDeclaration> ElementTypeDecl = FormatTypeName( ElementType, TypeProvider );
        const std::shared_ptr<ArrayTypeDeclaration> ArrayTypeDecl = std::make_shared<ArrayTypeDeclaration>();
        ArrayTypeDecl->ElementType = ElementTypeDecl;
        if ( ArrayCount != 0 )
        {
            ArrayTypeDecl->ArrayDimension = ArrayCount;
        }
        return ArrayTypeDecl;
    }
    if ( SymbolTag == SymTagBaseType )
    {
        const DWORD BaseType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, baseType );
        const LONGLONG BaseTypeSize = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, length );
        const BOOL bIsBaseTypeConst = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, constType );

        const std::shared_ptr<ITypeDeclaration> BasicTypeDecl = FormatBasicTypeName( BaseType, BaseTypeSize );
        BasicTypeDecl->bIsConst = bIsBaseTypeConst;
        return BasicTypeDecl;
    }
    if ( SymbolTag == SymTagCustomType )
    {
        const DWORD VendorDefinedTypeId = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, oemId );
        const SymbolNameInfo CustomTypeName = SymbolNameInfo::FromSymbol( InTypeSymbol );

        const std::shared_ptr<UDTTypeDeclaration> TypeDecl = std::make_shared<UDTTypeDeclaration>();
        TypeDecl->OuterScope = CustomTypeName.SymbolScope;
        TypeDecl->ClassName = CustomTypeName.LocalName.empty() ? StringPrintf(L"OEM_TYPE_%d", VendorDefinedTypeId) : CustomTypeName.LocalName;
        return TypeDecl;
    }
    if ( SymbolTag == SymTagEnum )
    {
        // Enumerations are named user defined types. We could use underlying basic type here, but we prefer to just emit the enumeration name instead
        const BOOL bIsEnumConst = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, constType );
        const SymbolNameInfo EnumerationNameInfo = SymbolNameInfo::FromSymbol( InTypeSymbol );

        // If this is an anonymous type, generate it
        if ( EnumerationNameInfo.bIsAnonymousSymbol )
        {
            const std::shared_ptr<AnonymousEnumTypeDeclaration> TypeDecl = std::make_shared<AnonymousEnumTypeDeclaration>();
            TypeDecl->Data = GeneratedHeaderFile::MakeEnum( InTypeSymbol, nullptr, TypeProvider );
            TypeDecl->bIsConst = bIsEnumConst;
            return TypeDecl;
        }

        // This is a non-anonymous enumeration symbol, generate the type name
        const CComPtr<IDiaSymbol> UnderlyingType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, type );
        const BOOL bIsEnumScoped = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, scoped );

        const std::shared_ptr<EnumTypeDeclaration> TypeDecl = std::make_shared<EnumTypeDeclaration>();
        TypeDecl->OuterScope = EnumerationNameInfo.SymbolScope;
        TypeDecl->EnumName = EnumerationNameInfo.LocalName;
        TypeDecl->bIsConst = bIsEnumConst;

        // Attach additional data in case we want to emit enum predeclaration
        TypeDecl->UnderlyingType = FormatTypeName( UnderlyingType, TypeProvider );
        TypeDecl->bIsEnumClass = bIsEnumScoped;
        return TypeDecl;
    }
    if ( SymbolTag == SymTagPointerType )
    {
        const CComPtr<IDiaSymbol> PointeeType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, type );
        const BOOL bIsPointerConst = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, constType );
        const BOOL bIsPointerReference = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, reference );
        const DWORD PointeeSymTag = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( PointeeType, symTag );

        // If we are pointing at the function signature, we do not need to do anything, just let it handle itself. Non-pointer function signatures are not a thing.
        if ( PointeeSymTag == SymTagFunctionType )
        {
            const std::shared_ptr<ITypeDeclaration> FunctionSignatureType = FormatTypeName( PointeeType, TypeProvider );
            assert( FunctionSignatureType->GetId() == ETypeDeclarationId::FunctionType );
            static_cast<FunctionTypeDeclaration*>( FunctionSignatureType.get() )->bIsFunctionPointer = true;
            return FunctionSignatureType;
        }

        const std::shared_ptr<ITypeDeclaration> PointeeTypeDecl = FormatTypeName( PointeeType, TypeProvider );

        const std::shared_ptr<PointerTypeDeclaration> PointerTypeDecl = std::make_shared<PointerTypeDeclaration>();
        PointerTypeDecl->PointeeType = PointeeTypeDecl;
        PointerTypeDecl->bIsReference = bIsPointerReference;
        PointerTypeDecl->bIsConst = bIsPointerConst;
        return PointerTypeDecl;
    }
    if ( SymbolTag == SymTagTypedef )
    {
        // TODO: Typedefs require a separate type that needs to be transparent, e.g. retrospectable. For now, emit the underlying type instead
        const CComPtr<IDiaSymbol> TypedefSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, type );
        return FormatTypeName( TypedefSymbol, TypeProvider );
    }
    if ( SymbolTag == SymTagFunctionType )
    {
        const std::shared_ptr<FunctionTypeDeclaration> FunctionSignatureType = std::make_shared<FunctionTypeDeclaration>();
        FormatFunctionType( InTypeSymbol, TypeProvider, FunctionSignatureType->ReturnType, FunctionSignatureType->Arguments, FunctionSignatureType->bIsVariadicArguments,
            &FunctionSignatureType->OwnerType, nullptr, &FunctionSignatureType->bIsConstMemberFunction );

        return FunctionSignatureType;
    }
    if ( SymbolTag == SymTagUDT )
    {
        const SymbolNameInfo UserDefinedTypeName = SymbolNameInfo::FromSymbol( InTypeSymbol );
        const BOOL bIsTypeConst = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, constType );

        // If this is an anonymous type, generate it
        if ( UserDefinedTypeName.bIsAnonymousSymbol )
        {
            const std::shared_ptr<AnonymousUDTTypeDeclaration> TypeDecl = std::make_shared<AnonymousUDTTypeDeclaration>();
            TypeDecl->Data = GeneratedHeaderFile::MakeUDT( InTypeSymbol, nullptr, nullptr, TypeProvider );
            TypeDecl->bIsConst = bIsTypeConst;
            return TypeDecl;
        }

        // This is a non-anonymous UDT symbol, generate the type name
        const DWORD TypeUDTKind = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, udtKind );
        const std::shared_ptr<UDTTypeDeclaration> TypeDecl = std::make_shared<UDTTypeDeclaration>();
        TypeDecl->OuterScope = UserDefinedTypeName.SymbolScope;
        TypeDecl->ClassName = UserDefinedTypeName.LocalName;
        TypeDecl->bIsConst = bIsTypeConst;

        // Parse template instantiation arguments. This will allow us to pretty-print them and also gather their dependencies,
        // as well as substitute more verbose template instantiations with simpler declarations
        // TODO: This will not correctly generate template arguments for nested types declared inside of namespaced types.
        if ( UserDefinedTypeName.bIsTemplateInstantiation )
        {
            TypeTemplateArgumentContainer TemplateArguments;
            assert( TypeTemplateArgumentContainer::ParseTemplateArguments( UserDefinedTypeName.TemplateArguments, TemplateArguments ) && L"Failed to parse template instantiation arguments for UDT type" );
            PatchupInternalTypeReferences( TemplateArguments, InTypeSymbol );
            TypeDecl->TemplateArguments = TemplateArguments;
        }

        // Potentially remap type name to another type name with less verbose template instantiation
        std::wstring ReplacementNamespace, ReplacementClassName;
        TypeTemplateArgumentContainer ReplacementTemplate;
        if ( TypeProvider->RemapTypeReference( TypeDecl->OuterScope, TypeDecl->ClassName, TypeDecl->TemplateArguments, ReplacementNamespace, ReplacementClassName, ReplacementTemplate ) )
        {
            TypeDecl->OuterType = nullptr;
            TypeDecl->OuterScope = ReplacementNamespace;
            TypeDecl->ClassName = ReplacementClassName;
            TypeDecl->TemplateArguments = ReplacementTemplate;
        }

        // Attach additional data in case we need to generate a predeclaration
        TypeDecl->UDTKind = ConvertUDTKind( TypeUDTKind );
        return TypeDecl;
    }
    assert(!L"unsupported type symbol tag");
    return nullptr;
}

bool CodeGeneration::GenerateDefaultValueForSimpleType(const CComPtr<IDiaSymbol>& InTypeSymbol, std::wstring& OutDefaultValue)
{
    const DWORD SymbolTag = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, symTag );

    // We do not need to handle arrays because functions cannot return arrays.
    // Enumerations need to be casted to their type because they can be an enumeration class
    if ( SymbolTag == SymTagEnum )
    {
        const SymbolNameInfo EnumerationNameInfo = SymbolNameInfo::FromSymbol( InTypeSymbol );

        // Anonymous enumerations should never need to be casted to their type
        if ( EnumerationNameInfo.bIsAnonymousSymbol || EnumerationNameInfo.bIsUnnamedEnum )
        {
            OutDefaultValue = L"0";
        }
        else
        {
            // TODO: This will give ugly results for enumeration types nested into templated types, as they will not go through template expansion.
            OutDefaultValue = StringPrintf(L"static_cast<%s>(0)", EnumerationNameInfo.OriginalFullName.c_str());
        }
        return true;
    }
    // Nullptr is the default value for pointer types
    if ( SymbolTag == SymTagPointerType )
    {
        OutDefaultValue = L"nullptr";
        return true;
    }
    if ( SymbolTag == SymTagBaseType )
    {
        const DWORD BaseType = GET_SYMBOL_ATTRIBUTE_CHECKED( InTypeSymbol, baseType );
        if ( BaseType == btBool )
        {
            OutDefaultValue = L"false";
            return true;
        }
        if ( BaseType == btFloat )
        {
            OutDefaultValue = L"0.0f";
            return true;
        }
        if ( BaseType == btChar || BaseType == btInt || BaseType == btLong || BaseType == btUInt || BaseType == btULong )
        {
            OutDefaultValue = L"0";
            return true;
        }
        if ( BaseType == btWChar || BaseType == btChar8 || BaseType == btChar16 || BaseType == btChar32 )
        {
            OutDefaultValue = L"'\0'";
            return true;
        }
    }
    return false;
}

void CodeGeneration::FormatFunctionType(const CComPtr<IDiaSymbol>& InFunctionSignatureType, ITypeResolutionProvider* TypeProvider,
                                        std::shared_ptr<ITypeDeclaration>& OutReturnType,
                                        std::vector<std::pair<std::wstring, std::shared_ptr<ITypeDeclaration>>>& OutArgumentNamesAndTypes,
                                        bool& OutIsVariadicArguments,
                                        std::shared_ptr<ITypeDeclaration>* OutThisPointerType, const CComPtr<IDiaSymbol>& InOwnerFunction,
                                        bool* OutIsConstMemberFunction)
{
     assert( GET_SYMBOL_ATTRIBUTE_CHECKED( InFunctionSignatureType, symTag ) == SymTagFunctionType );

    if ( const CComPtr<IDiaSymbol> FunctionReturnType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InFunctionSignatureType, type ) )
    {
        // Some template instantion function return values somehow have btNoType as their return value? Observed on std::_Tree::insert specialization. original function returns either iterator or void
        bool bHasInvalidReturnValue = false;
        if ( const DWORD SymbolTag = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionReturnType, symTag ); SymbolTag == SymTagBaseType )
        {
            if ( const DWORD BaseTypeTag = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionReturnType, baseType ); BaseTypeTag == btNoType )
            {
                const std::wstring FunctionName = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InOwnerFunction, name );
                std::wcerr << L"Return value with invalid type (<NoType>) was found when generating signature for function " << FunctionName << L". Substituing with void" << std::endl;
                bHasInvalidReturnValue = true;
            }
        }

        if ( bHasInvalidReturnValue )
        {
            OutReturnType = std::make_shared<VoidTypeDeclaration>();
        }
        else
        {
            OutReturnType = FormatTypeName( FunctionReturnType, TypeProvider );
        }
    }
    else
    {
        OutReturnType = std::make_shared<VoidTypeDeclaration>();
    }

    bool bHasObjectPointerArgument = false;
    if ( const CComPtr<IDiaSymbol> ObjectPointerType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InFunctionSignatureType, objectPointerType ) )
    {
        // Object pointer type should always be a pointer to the UDT
        const DWORD ObjectPointerSymTag = GET_SYMBOL_ATTRIBUTE_CHECKED( ObjectPointerType, symTag );
        assert( ObjectPointerSymTag == SymTagPointerType );
        bHasObjectPointerArgument = true;

        const CComPtr<IDiaSymbol> CVModifiedObjectType = GET_SYMBOL_ATTRIBUTE_CHECKED( ObjectPointerType, type );
        const CComPtr<IDiaSymbol> ThisObjectType = DiaUtils::RemoveCVModifiersFromType( CVModifiedObjectType );
        if ( OutIsConstMemberFunction )
        {
            *OutIsConstMemberFunction = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( CVModifiedObjectType, constType );
        }
        if ( OutThisPointerType )
        {
            *OutThisPointerType = FormatTypeName( ThisObjectType, TypeProvider );
        }
    }

    bool bIsVariadicFunction = false;
    std::vector<DWORD> ArgumentTypeSymbolIds;
    for ( DiaChildSymbolIterator It( InFunctionSignatureType, SymTagFunctionArgType ); It; ++It )
    {
        const CComPtr<IDiaSymbol> FunctionArgumentSymbol = *It;
        const CComPtr<IDiaSymbol> FunctionArgumentType = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionArgumentSymbol, type );

        // Check against vararg function arguments. Variadic arguments (C style) will appear as argument with no type information
        if (const DWORD ArgumentTypeSymTag = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionArgumentType, symTag ); ArgumentTypeSymTag == SymTagBaseType )
        {
            if (const DWORD BaseTypeKind = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionArgumentType, baseType ); BaseTypeKind == btNoType )
            {
                assert( !bIsVariadicFunction );
                bIsVariadicFunction = true;
                continue;
            }
        }

        const DWORD TypeSymbolId = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionArgumentType, symIndexId );
        const std::shared_ptr<ITypeDeclaration> ArgumentType = FormatTypeName( FunctionArgumentType, TypeProvider );

        // Push the argument with no name, and record it's type so we can potentially match it up with a name using local variable lookup later
        OutArgumentNamesAndTypes.push_back({L"", ArgumentType});
        ArgumentTypeSymbolIds.push_back(TypeSymbolId);
    }

    // Attempt to match a name with the argument by looking up at the data variables defined in the function
    std::wstring FunctionArgumentName;
    if ( InOwnerFunction )
    {
        // Only local variables defined directly inside of the function should be it's parameters, any user-defined local variables would be children of the blocks and not the function directly
        int32_t CurrentArgumentIndex = 0;
        for ( DiaChildSymbolIterator LocalVariableIt( InOwnerFunction, SymTagData ); LocalVariableIt && CurrentArgumentIndex < ArgumentTypeSymbolIds.size(); ++LocalVariableIt )
        {
            // Skip over the first local variable if this is an instance function because it will be the object pointer argument, which is hidden and does not appear as a real function argument
            if ( bHasObjectPointerArgument && LocalVariableIt.GetCurrentIndex() == 0 ) continue;

            const CComPtr<IDiaSymbol> FunctionDataSymbol = *LocalVariableIt;
            const CComPtr<IDiaSymbol> LocalVariableType = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionDataSymbol, type );

            // All local variables should be either contained inside of the registers or relative to the RSP stack register
            const DWORD LocalVariableLocationType = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionDataSymbol, locationType );
            const DWORD LocalVariableTypeId = GET_SYMBOL_ATTRIBUTE_CHECKED( LocalVariableType, symIndexId );

            // Skip constants and static/thread local variables
            if ( LocalVariableLocationType == LocIsStatic || LocalVariableLocationType == LocIsTLS || LocalVariableLocationType == LocIsConstant ) continue;

            // The rest should be register relative and enrigestered
            assert( LocalVariableLocationType == LocIsRegRel || LocalVariableLocationType == LocIsEnregistered );

            // If the local variable type does not match the argument type, we failed to map the parameters to the names. But having incorrect names is better than having wrong names,
            // so we just skip it and continue instead of breaking out of the loop
            if ( ArgumentTypeSymbolIds[ CurrentArgumentIndex ] != LocalVariableTypeId ) continue;

            const std::wstring LocalVariableName = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionDataSymbol, name );
            OutArgumentNamesAndTypes[ CurrentArgumentIndex ].first = LocalVariableName;
            CurrentArgumentIndex++;
        }
    }

    // Add variadic argument last if we found one.
    OutIsVariadicArguments = bIsVariadicFunction;
}

void CodeGeneration::PatchupInternalTypeReferences(TypeTemplateArgumentContainer& TemplateArguments, const CComPtr<IDiaSymbol>& InTemplateTypeContext)
{
    // TODO: Implement. This would require pattern matching members inside of the template with anonymous/unnamed template arguments
}

std::shared_ptr<ITypeDeclaration> CodeGeneration::FormatBasicTypeName(const DWORD InBasicType, const LONGLONG InBasicTypeSizeBytes )
{
    const std::shared_ptr<FundamentalTypeDeclaration> ResultType = std::make_shared<FundamentalTypeDeclaration>();
    switch (InBasicType)
    {
        case btVoid:
            assert(InBasicTypeSizeBytes == 0 && L"void should be 0 in size");
            return std::make_shared<VoidTypeDeclaration>();
        case btChar:
            // TODO: Why is there no btUChar? Is there a way to know whenever the original character was signed or unsigned?
            assert(InBasicTypeSizeBytes == 1 && L"btChar should always be 1 byte long on all platforms");
            ResultType->BasicType = EBasicType::Char;
            return ResultType;
        case btWChar:
            assert((InBasicTypeSizeBytes == 2 || InBasicTypeSizeBytes == 4) && L"btWChar should be either 2 bytes on Windows or 4 bytes on other platforms, unusual size detected");
            ResultType->BasicType = EBasicType::WideChar;
            return ResultType;
        case btChar8:
            ResultType->BasicType = EBasicType::Char8;
            return ResultType;
        case btChar16:
            ResultType->BasicType = EBasicType::Char16;
            return ResultType;
        case btChar32:
            ResultType->BasicType = EBasicType::Char32;
            return ResultType;
        case btBool:
            // We do not know the size of this type, since MS docs describe it as BOOL, and BOOL is a define for int.
            // But it looks like thsi documentation is outdated, and btBool matches the size of the bool type, size of which is implementation-defined. So we emit bool here.
            // Still, let's make sure it's 1 byte in size for now to catch any unusual cases that might need special treatment
            assert( InBasicTypeSizeBytes == 1 );
            ResultType->BasicType = EBasicType::Bool;
            return ResultType;
        case btFloat:
            assert((InBasicTypeSizeBytes == 4 || InBasicTypeSizeBytes == 8) && L"btFloat has unusual size, should be either 4 bytes for float or 8 bytes for double");
            ResultType->BasicType = InBasicTypeSizeBytes == 8 ? EBasicType::Double : EBasicType::Float;
            return ResultType;
        case btInt:
        case btLong:
            // Use fixed length integer types since we know the actual byte size of the type
            ResultType->BasicType = GetFixedLengthIntType( InBasicTypeSizeBytes );
            ResultType->bIsUnsigned = false;
            return ResultType;
        case btUInt:
        case btULong:
            // Use fixed length integer types since we know the actual byte size of the type
            ResultType->BasicType = GetFixedLengthIntType( InBasicTypeSizeBytes );
            ResultType->bIsUnsigned = true;
            return ResultType;
        case btCurrency:
                // CURRENCY type from win32 API
                return std::make_shared<UDTTypeDeclaration>(L"CURRENCY");
        case btDate:
                // DATE type from win32 API
                return std::make_shared<UDTTypeDeclaration>(L"DATE");
        case btVariant:
                // VARIANT type from win32 API
                return std::make_shared<UDTTypeDeclaration>(L"VARIANT");
        case btBSTR:
                // BSTR type from win32 API
                return std::make_shared<UDTTypeDeclaration>(L"BSTR");
        case btHresult:
                // HRESULT type from win32 API
                return std::make_shared<UDTTypeDeclaration>(L"HRESULT");
        case btBCD:
            // ??? No clue what type this is, it does not seem to exist in the standard, MSVC stdlib or win32 API even
            assert(!L"btBCD (Binary Coded Decimal) types are not supported");
            return nullptr;
        case btComplex:
            // ??? No clue what type this is, it does not seem to exist in the standard, MSVC stdlib or win32 API even
            assert(!L"btComplex (complex float/double number type) is not a valid type in C++");
            return nullptr;
        case btBit:
            assert(!L"btBit type is not supported, there is no valid C++ type it can be reliably mapped to");
            return nullptr;
        default:
            assert(!L"unknown basic type");
            return nullptr;
    }
}
