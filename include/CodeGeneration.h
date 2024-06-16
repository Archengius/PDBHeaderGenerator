#pragma once

#include <cstdint>
#include <xstring>
#include <vector>
#include "Utils/DiaUtils.h"
#include "AST/CppTypeDeclaration.h"

class ITypeResolutionProvider;
enum class CppUDTKind : uint8_t;
enum class CppAccessModifier : uint8_t;

namespace CodeGeneration
{
    std::shared_ptr<ITypeDeclaration> FormatTypeName( const CComPtr<IDiaSymbol>& InTypeSymbol, ITypeResolutionProvider* TypeProvider );
    void FormatFunctionType( const CComPtr<IDiaSymbol>& InFunctionSignatureType, ITypeResolutionProvider* TypeProvider, std::shared_ptr<ITypeDeclaration>& OutReturnType, std::vector<std::pair<std::wstring, std::shared_ptr<ITypeDeclaration>>>& OutArgumentNamesAndTypes,
        bool& OutIsVariadicArguments,
        std::shared_ptr<ITypeDeclaration>* OutThisPointerType = nullptr,
        const CComPtr<IDiaSymbol>& InOwnerFunction = nullptr,
        bool* OutIsConstMemberFunction = nullptr );
    std::shared_ptr<ITypeDeclaration> FormatBasicTypeName(DWORD InBasicType, LONGLONG InBasicTypeSizeBytes );
    bool GenerateDefaultValueForSimpleType( const CComPtr<IDiaSymbol>& InTypeSymbol, std::wstring& OutDefaultValue );

    void PatchupInternalTypeReferences( TypeTemplateArgumentContainer& TemplateArguments, const CComPtr<IDiaSymbol>& InTemplateTypeContext );

    CppAccessModifier ConvertAccessModifier( DWORD InAccessModifier );
    CppUDTKind ConvertUDTKind( DWORD InUDTKind );
    bool IsInternalSymbolName( const SymbolNameInfo& SymbolName );

    std::wstring GenerateConstantValue( const VARIANT& InVariant );
}
