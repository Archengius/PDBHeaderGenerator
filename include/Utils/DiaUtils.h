#pragma once

#include <cstdint>

#define WIN32_LEAN_AND_MEAN
#include <comdef.h>
#include <atlbase.h>
#include <dia2.h>
#include <string>

#define UNDNAME_NAME_ONLY 0x1000

namespace SymbolAttributeHelpers
{
    template<typename TCallable>
    struct DiaSymbolAttributeTypeRetriever;

    template<typename InAttributeType>
    struct DiaSymbolAttributeTypeRetriever<HRESULT(IDiaSymbol::*)(InAttributeType*)>
    {
        using AttributeType = InAttributeType;
    };

    template<typename InAttributeType>
    struct DiaSymbolAttributeRetriever;

    // Implementation for non-pointers, does not do any kind of additional freeing or reference counting
    template<typename InAttributeType>
    requires(!std::is_pointer_v<InAttributeType>)
    struct DiaSymbolAttributeRetriever<InAttributeType>
    {
        InAttributeType AttributeData{};

        InAttributeType* GetAttributePtrForWrite()
        {
            return &AttributeData;
        }
        InAttributeType CopyAttributeForRead() const
        {
            return AttributeData;
        }
    };

    // Implementation for BSTR
    template<>
    struct DiaSymbolAttributeRetriever<BSTR>
    {
        BSTR AttributeData{};

        ~DiaSymbolAttributeRetriever()
        {
            SysFreeString(AttributeData);
        }

        BSTR* GetAttributePtrForWrite()
        {
            return &AttributeData;
        }
        std::wstring CopyAttributeForRead() const
        {
            if ( AttributeData == nullptr )
            {
                return std::wstring();
            }
            return std::wstring( AttributeData );
        }
    };

    // Implementation for COM types (child of IUnknown)
    template<typename ComType>
    requires(std::is_base_of_v<IUnknown, ComType>)
    struct DiaSymbolAttributeRetriever<ComType*>
    {
        CComPtr<ComType> AttributeData;

        ComType** GetAttributePtrForWrite()
        {
            return &AttributeData;
        }
        CComPtr<ComType> CopyAttributeForRead() const
        {
            return AttributeData;
        }
    };

    // Implementation for VARIANT
    template<>
    struct DiaSymbolAttributeRetriever<VARIANT>
    {
        CComVariant AttributeData;

        VARIANT* GetAttributePtrForWrite()
        {
            return &AttributeData;
        }
        CComVariant CopyAttributeForRead() const
        {
            return AttributeData;
        }
    };
}

#define GET_SYMBOL_ATTRIBUTE_INTERNAL( Symbol, AttributeName, bChecked ) \
    ([&](){ \
        using AttributeType = SymbolAttributeHelpers::DiaSymbolAttributeTypeRetriever<decltype(&IDiaSymbol::get_##AttributeName)>::AttributeType; \
        SymbolAttributeHelpers::DiaSymbolAttributeRetriever<AttributeType> AttributeRetriever; \
        const HRESULT RetrievalResult = Symbol ? Symbol->get_##AttributeName( AttributeRetriever.GetAttributePtrForWrite() ) : S_FALSE; \
        assert( SUCCEEDED( RetrievalResult ) ); \
        assert( !bChecked || RetrievalResult == S_OK ); \
        return AttributeRetriever.CopyAttributeForRead(); \
    })()

#define GET_SYMBOL_ATTRIBUTE_CHECKED( Symbol, AttributeName ) GET_SYMBOL_ATTRIBUTE_INTERNAL( Symbol, AttributeName, true )
#define GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( Symbol, AttributeName ) GET_SYMBOL_ATTRIBUTE_INTERNAL( Symbol, AttributeName, false )

/** Creates DIA data source from the DLL module handle */
HRESULT CoCreateDiaDataSource(HMODULE DiaDllHandle, CComPtr<IDiaDataSource>& OutDataDataSource);

/** Simple iterator to iterate on DIA symbols */
class DiaChildSymbolIterator
{
    CComPtr<IDiaEnumSymbols> UnderlyingEnumerator;
    LONG ItemCount{0};
    LONG CurrentIndex{0};
public:
    DiaChildSymbolIterator( const CComPtr<IDiaSymbol>& InParentSymbol, enum SymTagEnum InSymbolType = SymTagNull, const WCHAR* InName = nullptr, DWORD CompareFlags = nsNone );
    DiaChildSymbolIterator( const CComPtr<IDiaSymbol>& InParentSymbol, enum SymTagEnum InSymbolType, const WCHAR* InName, DWORD CompareFlags, DWORD SymbolRVA );

    FORCEINLINE LONG GetCurrentIndex() const { return CurrentIndex; }
    // ReSharper disable once CppNonExplicitConversionOperator
    operator bool() const;
    CComPtr<IDiaSymbol> operator*() const;
    void operator++();
};

struct SymbolNameInfo
{
    // Scope of the symbol. That would be the namespace, or class name (including namespace), depending on symbol place in the hierarchy
    std::wstring SymbolScope;
    // Local name of the symbol inside of it's scope. For example, that would be the raw name of the function inside of the class, or raw name of the class without the namespace information
    std::wstring LocalName;
    // If this is a template, this is the arguments that were passed to the template instantiation
    std::wstring TemplateArguments;
    // True if this symbol is a template instantiation. Needs to be separate from TemplateArguments because templates can have no arguments
    bool bIsTemplateInstantiation{false};
    // True if this symbol is anonymous, such as an anonymous namespace or union. LocalName will be an internal name of the anonymous symbol. bIsUnnamedType or bIsUnnamedEnum can be additionally set
    bool bIsAnonymousSymbol{false};
    // True if this symbol is an unnamed type
    bool bIsUnnamedType{false};
    // True if this symbol is an unnamed enum
    bool bIsUnnamedEnum{false};
    // True if this symbol is a lambda
    bool bIsLambdaSymbol{false};
    // Original name of the symbol, as it was retrieved from the symbol data
    std::wstring OriginalFullName;
    // The variable name that caused the generation of this anonymous symbol.
    std::wstring AnonymousSymbolOriginVariableName;

    enum SymbolNameInfoToStringOptions : uint32_t
    {
        IncludeLocalNameOnly = 0,
        IncludeNamespace = 0x1,
        IncludeTemplateArguments = 0x2,
        IncludeAll = IncludeNamespace | IncludeTemplateArguments
    };

    // Converts the symbol name struct back into the original name
    std::wstring ToString( int32_t InOptions = IncludeAll ) const;

    // Constructs the symbol name info for the symbol
    static SymbolNameInfo FromSymbol( const CComPtr<IDiaSymbol>& InSymbol );
    static SymbolNameInfo FromSymbolName( std::wstring SymbolName );
};

namespace DiaUtils
{
    CComPtr<IDiaSymbol> RemoveCVModifiersFromType( const CComPtr<IDiaSymbol>& InTypeSymbol );

    bool AreFunctionTypesEquivalent( const CComPtr<IDiaSymbol>& InFunctionTypeA, const CComPtr<IDiaSymbol>& InFunctionTypeB, bool bCheckObjectPointer );
    CComPtr<IDiaSymbol> FindParentVirtualFunction( const CComPtr<IDiaSymbol>& InTypeSymbol, const std::wstring& FunctionName, const CComPtr<IDiaSymbol>& InFunctionSignature );

    bool IsFunctionImplementationTheSame( const CComPtr<IDiaSymbol>& FunctionA, const CComPtr<IDiaSymbol>& FunctionB );
    std::wstring GetSymbolUndecoratedName( const CComPtr<IDiaSymbol>& InSymbol, DWORD UndecorateOptions = 0 );
}
