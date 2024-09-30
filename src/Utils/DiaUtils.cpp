#include "Utils/DiaUtils.h"

#include <sstream>
#include <vector>
#include <unordered_set>

HRESULT CoCreateDiaDataSource(const HMODULE DiaDllHandle, CComPtr<IDiaDataSource>& OutDataDataSource)
{
    const auto DllGetClassObject = reinterpret_cast<LPFNGETCLASSOBJECT>( GetProcAddress( DiaDllHandle, "DllGetClassObject" ) );
    if (!DllGetClassObject)
    {
        return HRESULT_FROM_WIN32(GetLastError());
    }
    CComPtr<IClassFactory> pClassFactory;
    if ( const HRESULT Result = DllGetClassObject(CLSID_DiaSource, IID_IClassFactory, reinterpret_cast<LPVOID*>(&pClassFactory)); FAILED(Result) )
    {
        return Result;
    }
    if ( const HRESULT Result = pClassFactory->CreateInstance(nullptr, IID_IDiaDataSource, (void **) &OutDataDataSource); FAILED(Result) )
    {
        return Result;
    }
    return S_OK;
}

DiaChildSymbolIterator::DiaChildSymbolIterator( const CComPtr<IDiaSymbol>& InParentSymbol, enum SymTagEnum InSymbolType, const WCHAR* InName, DWORD CompareFlags )
{
    const HRESULT EnumeratorOpenResult = InParentSymbol->findChildren(InSymbolType, InName, CompareFlags, &UnderlyingEnumerator);
    assert( SUCCEEDED( EnumeratorOpenResult ) );
    assert( SUCCEEDED( UnderlyingEnumerator->get_Count( &ItemCount ) ) );
}

DiaChildSymbolIterator::DiaChildSymbolIterator(const CComPtr<IDiaSymbol>& InParentSymbol, enum SymTagEnum InSymbolType, const WCHAR* InName, DWORD CompareFlags, DWORD SymbolRVA)
{
    const HRESULT EnumeratorOpenResult = InParentSymbol->findChildrenExByRVA(InSymbolType, InName, CompareFlags, SymbolRVA, &UnderlyingEnumerator);
    assert( SUCCEEDED( EnumeratorOpenResult ) );
    assert( SUCCEEDED( UnderlyingEnumerator->get_Count( &ItemCount ) ) );
}

DiaChildSymbolIterator::operator bool() const
{
    return CurrentIndex < ItemCount;
}

CComPtr<IDiaSymbol> DiaChildSymbolIterator::operator*() const
{
    CComPtr<IDiaSymbol> NextSymbol;
    assert( SUCCEEDED( UnderlyingEnumerator->Item( CurrentIndex, &NextSymbol ) ) );
    return NextSymbol;
}

void DiaChildSymbolIterator::operator++()
{
    CurrentIndex++;
}

static size_t FindTemplateArgumentsStartIndex( const std::wstring& InFullName, const size_t TemplateEndIndex )
{
    assert( InFullName[ TemplateEndIndex ] == L'>' );

    int32_t CurrentIndex = TemplateEndIndex - 1;
    int32_t CurrentNestingLevel = 1;

    while ( CurrentIndex >= 0 && CurrentNestingLevel > 0 )
    {
        if ( InFullName[ CurrentIndex ] == L'<' )
        {
            CurrentNestingLevel--;
        }
        else if ( InFullName[ CurrentIndex ] == L'>' )
        {
            CurrentNestingLevel++;
        }
        CurrentIndex--;
    }
    // We will overshoot by 1 character to the left
    return CurrentNestingLevel == 0 ? ( CurrentIndex + 1 ) : std::wstring::npos;
}

SymbolNameInfo SymbolNameInfo::FromSymbol(const CComPtr<IDiaSymbol>& InSymbol)
{
    const std::wstring SymbolName = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InSymbol, name );
    return FromSymbolName( SymbolName );
}

SymbolNameInfo SymbolNameInfo::FromSymbolName(std::wstring SymbolName)
{
    SymbolNameInfo ResultNameInfo;
    ResultNameInfo.OriginalFullName = SymbolName;

    // Symbol with an empty name. Skip any kind of name parsing
    if ( SymbolName.empty() )
    {
        return ResultNameInfo;
    }

    // Make sure not to cut off namespaces inside of the template arguments. So look for the first template argument start
    // Special case to not accidentally treat operator-> as template specialization
    if ( SymbolName[ SymbolName.size() - 1 ] == L'>' &&
        !SymbolName.ends_with(L"operator->") && !SymbolName.ends_with(L"operator>") && !SymbolName.ends_with(L"operator>>") &&
        !SymbolName.ends_with(L"operator>=") && !SymbolName.ends_with(L"operator<=>") )
    {
        const size_t ArgumentStartIndex = FindTemplateArgumentsStartIndex( SymbolName, SymbolName.size() - 1 );
        assert( ArgumentStartIndex != std::wstring::npos && L"Invalid nesting depth inside of the template specialization name" );

        const std::wstring TemplateArgumentsOrAnonymousTypeName = SymbolName.substr( ArgumentStartIndex + 1, SymbolName.size() - (ArgumentStartIndex + 1) - 1 );

        // Check if this is a nameless anonymous type that does not indicate which field it comes from. These do not have much information on them.
        if ( TemplateArgumentsOrAnonymousTypeName == L"anonymous-tag" || TemplateArgumentsOrAnonymousTypeName == L"unnamed-tag" )
        {
            ResultNameInfo.bIsAnonymousSymbol = true;
        }
        // Check for unnamed types with variable name in them
        else if ( TemplateArgumentsOrAnonymousTypeName.starts_with(L"unnamed-type-") )
        {
            ResultNameInfo.bIsAnonymousSymbol = true;
            ResultNameInfo.bIsUnnamedType = true;
            ResultNameInfo.AnonymousSymbolOriginVariableName = TemplateArgumentsOrAnonymousTypeName.substr( 13 );
        }
        // Check for unnamed enums with variable name in them
        else if ( TemplateArgumentsOrAnonymousTypeName.starts_with(L"unnamed-enum-") )
        {
            ResultNameInfo.bIsAnonymousSymbol = true;
            ResultNameInfo.bIsUnnamedEnum = true;
            ResultNameInfo.AnonymousSymbolOriginVariableName = TemplateArgumentsOrAnonymousTypeName.substr( 13 );
        }
        // Check if this is a lambda
        else if ( TemplateArgumentsOrAnonymousTypeName.starts_with(L"lambda_") )
        {
            ResultNameInfo.bIsLambdaSymbol = true;
        }
        // Otherwise, it is a normal template instantiation
        else
        {
            ResultNameInfo.bIsTemplateInstantiation = true;
            ResultNameInfo.TemplateArguments = TemplateArgumentsOrAnonymousTypeName;
            SymbolName = SymbolName.substr( 0, ArgumentStartIndex );
        }
    }

    // Look for the namespace now
    if ( const size_t LastNamespaceIndex = SymbolName.rfind(L"::"); LastNamespaceIndex != std::wstring::npos )
    {
        ResultNameInfo.SymbolScope = SymbolName.substr( 0, LastNamespaceIndex );
        SymbolName = SymbolName.substr( LastNamespaceIndex + 2 );
    }

    // The remaining bit is a local symbol name
    ResultNameInfo.LocalName = SymbolName;
    return ResultNameInfo;
}

std::wstring SymbolNameInfo::ToString( const int32_t InOptions ) const
{
    std::wostringstream ResultStr;

    if ( !SymbolScope.empty() && ( InOptions & IncludeNamespace ) != 0 )
    {
        ResultStr << SymbolScope << L"::";
    }
    ResultStr << LocalName;

    if ( bIsTemplateInstantiation && ( InOptions & IncludeTemplateArguments ) != 0 )
    {
        ResultStr << L"<" << TemplateArguments << L">";
    }
    return ResultStr.str();
}

CComPtr<IDiaSymbol> DiaUtils::RemoveCVModifiersFromType(const CComPtr<IDiaSymbol>& InTypeSymbol)
{
    // Retrieve the unmodified type first
    if ( const CComPtr<IDiaSymbol> UnmodifiedTypeSymbol = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InTypeSymbol, unmodifiedType ) )
    {
        return UnmodifiedTypeSymbol;
    }
    return InTypeSymbol;
}

bool DiaUtils::IsFunctionImplementationTheSame(const CComPtr<IDiaSymbol>& FunctionA, const CComPtr<IDiaSymbol>& FunctionB)
{
    const DWORD LocationTypeA = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionA, locationType );
    const DWORD LocationTypeB = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionB, locationType );

    if ( LocationTypeA != LocationTypeB ) return false;
    assert( LocationTypeA == LocIsNull || LocationTypeA == LocIsStatic );

    if ( LocationTypeA == LocIsStatic )
    {
        const DWORD RelativeVirtualAddressA = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionA, relativeVirtualAddress );
        const DWORD RelativeVirtualAddressB = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionB, relativeVirtualAddress );

        // Implementation is the same if both functions point to the same virtual address
        return RelativeVirtualAddressA == RelativeVirtualAddressB;
    }
    if ( LocationTypeA == LocIsNull )
    {
        return true;
    }
    return false;
}

std::wstring DiaUtils::GetSymbolUndecoratedName(const CComPtr<IDiaSymbol>& InSymbol, DWORD UndecorateOptions)
{
    BSTR UndecoratedName = nullptr;
    const HRESULT RetrievalResult = InSymbol->get_undecoratedNameEx( UndecorateOptions, &UndecoratedName );
    assert( SUCCEEDED( RetrievalResult ) );
    return std::wstring( UndecoratedName );
}

CComPtr<IDiaSymbol> DiaUtils::FindParentVirtualFunction(const CComPtr<IDiaSymbol>& InTypeSymbol, const std::wstring& FunctionName, const CComPtr<IDiaSymbol>& InFunctionSignature)
{
    std::unordered_set<DWORD> VisitedBaseClassTypes;
    std::vector<CComPtr<IDiaSymbol>> BaseClassTypesExploded;

    // Recursively explore all UDTs in this class hierarchy
    BaseClassTypesExploded.push_back( InTypeSymbol );
    for ( int32_t CurrentTypeIndex = 0; CurrentTypeIndex < BaseClassTypesExploded.size(); CurrentTypeIndex++ )
    {
        const CComPtr<IDiaSymbol> BaseClassTypeSymbol = BaseClassTypesExploded[ CurrentTypeIndex ];

        for ( DiaChildSymbolIterator It( BaseClassTypeSymbol, SymTagBaseClass ); It; ++It )
        {
            const CComPtr<IDiaSymbol> BaseClassSymbol = *It;
            CComPtr<IDiaSymbol> ParentClassTypeSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( BaseClassSymbol, type );

            if ( const CComPtr<IDiaSymbol> UnmodifiedTypeSymbol = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( ParentClassTypeSymbol, unmodifiedType ) )
            {
                ParentClassTypeSymbol = UnmodifiedTypeSymbol;
            }
            if (const DWORD ParentClassSymbolId = GET_SYMBOL_ATTRIBUTE_CHECKED( ParentClassTypeSymbol, symIndexId ); !VisitedBaseClassTypes.contains( ParentClassSymbolId ) )
            {
                VisitedBaseClassTypes.insert( ParentClassSymbolId );
                BaseClassTypesExploded.push_back( ParentClassTypeSymbol );
            }
        }
    }

    // Attempt to find the function with the same name and matching signature in each of the classes
    for ( const CComPtr<IDiaSymbol>& ParentClassSymbol : BaseClassTypesExploded )
    {
        // Skip the original type
        if ( ParentClassSymbol == InTypeSymbol ) continue;

        for ( DiaChildSymbolIterator It( ParentClassSymbol, SymTagFunction, FunctionName.c_str() ); It; ++It )
        {
            const CComPtr<IDiaSymbol> FunctionSymbol = *It;
            const BOOL bIsVirtualFunction = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( FunctionSymbol, virtual );
            const CComPtr<IDiaSymbol> FunctionTypeSymbol = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionSymbol, type );

            // Skip non-virtual functions, they might have the same name and even the same signature
            if ( !bIsVirtualFunction ) continue;

            // Skip special functions that do not have a valid type
            // ReSharper disable once CppTooWideScopeInitStatement
            const DWORD FunctionTypeSymbolTag = GET_SYMBOL_ATTRIBUTE_CHECKED( FunctionTypeSymbol, symTag );
            if ( FunctionTypeSymbolTag != SymTagFunctionType ) continue;

            if ( AreFunctionTypesEquivalent( InFunctionSignature, FunctionTypeSymbol, false ) )
            {
                return FunctionSymbol;
            }
        }
    }
    return nullptr;
}

bool DiaUtils::AreFunctionTypesEquivalent(const CComPtr<IDiaSymbol>& InFunctionTypeA, const CComPtr<IDiaSymbol>& InFunctionTypeB, const bool bCheckObjectPointer)
{
    const CComPtr<IDiaSymbol> ThisObjectPointerTypeA = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InFunctionTypeA, objectPointerType );
    const CComPtr<IDiaSymbol> ThisObjectPointerTypeB = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( InFunctionTypeA, objectPointerType );

    // Either both functions should have object pointer or neither
    if ( ( ThisObjectPointerTypeA == nullptr ) != ( ThisObjectPointerTypeB == nullptr ) ) return false;

    if ( bCheckObjectPointer )
    {
        // If we are asked to check if object pointers are matching, check that they point to the same symbol index
        if ( GET_SYMBOL_ATTRIBUTE_CHECKED( ThisObjectPointerTypeA, symIndexId ) != GET_SYMBOL_ATTRIBUTE_CHECKED( ThisObjectPointerTypeB, symIndexId ) ) return false;
    }

    std::vector<CComPtr<IDiaSymbol>> ArgumentTypesA;
    std::vector<CComPtr<IDiaSymbol>> ArgumentTypesB;

    for ( DiaChildSymbolIterator It( InFunctionTypeA, SymTagFunctionArgType ); It; ++It )
    {
        const CComPtr<IDiaSymbol> ArgumentSymbol = *It;
        const CComPtr<IDiaSymbol> ArgumentTypeSymbol = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( ArgumentSymbol, type );
        ArgumentTypesA.push_back( ArgumentTypeSymbol );
    }
    for ( DiaChildSymbolIterator It( InFunctionTypeB, SymTagFunctionArgType ); It; ++It )
    {
        const CComPtr<IDiaSymbol> ArgumentSymbol = *It;
        const CComPtr<IDiaSymbol> ArgumentTypeSymbol = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( ArgumentSymbol, type );
        ArgumentTypesB.push_back( ArgumentTypeSymbol );
    }

    // Argument count should always match
    if ( ArgumentTypesA.size() != ArgumentTypesB.size() ) return false;

    for ( int32_t ArgumentIndex = 0; ArgumentIndex < ArgumentTypesA.size(); ArgumentIndex++ )
    {
        const DWORD ArgumentTypeIdA = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( ArgumentTypesA[ ArgumentIndex ], symIndexId );
        const DWORD ArgumentTypeIdB = GET_SYMBOL_ATTRIBUTE_OR_DEFAULT( ArgumentTypesB[ ArgumentIndex ], symIndexId );

        // Argument indices should match
        if ( ArgumentTypeIdA != ArgumentTypeIdB ) return false;
    }
    return true;
}
