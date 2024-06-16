#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <AST/CppTypeDeclaration.h>

#include "Utils/DiaUtils.h"

class TypeDependency
{
    CComPtr<IDiaSymbol> DependencySymbol;
    DWORD SymbolId{};
    DWORD SymbolTag{};
    explicit TypeDependency( const CComPtr<IDiaSymbol>& InDependencySymbol );
public:
    FORCEINLINE bool IsEnum() const { return SymbolTag == SymTagEnum; }
    FORCEINLINE bool IsUserDefinedType() const { return SymbolTag == SymTagUDT; }
    FORCEINLINE DWORD GetSymbolUniqueId() const { return SymbolId; }
    // Retrieves the underlying symbol. Do not use it for comparison, DIA SDK instantiates symbols on demand, the same symbol ID can have multiple IDiaSymbol objects alive!
    FORCEINLINE CComPtr<IDiaSymbol> GetSymbol() const { return DependencySymbol; }

    static TypeDependency UserDefinedType( const CComPtr<IDiaSymbol>& UserDefinedType );
    static TypeDependency Enum( const CComPtr<IDiaSymbol>& Enum );

    friend bool operator==(const TypeDependency& A, const TypeDependency& B)
    {
        return A.GetSymbolUniqueId() == B.GetSymbolUniqueId();
    }
};

template<>
struct std::hash<TypeDependency>
{
    std::size_t operator()(const TypeDependency& TypeDependency) const noexcept
    {
        return std::hash<DWORD>{}(TypeDependency.GetSymbolUniqueId());
    }
};

struct PublicSymbolInfo
{
    bool bIsCLinkage{false};
};

class ITypeResolutionProvider
{
public:
    virtual ~ITypeResolutionProvider() = default;

    // Attempts to resolve the type by full name, which might contain a namespace or another type as a scope. Does not handle enumerations nested into templated types!!!!
    virtual CComPtr<IDiaSymbol> ResolveEnumByFullName( const std::wstring& InFullEnumName ) = 0;
    virtual CComPtr<IDiaSymbol> ResolveUDTByFullName( const std::wstring& InFullClassName ) = 0;
    // Attempts to resolve the type for a specific template instantiation given the template full name and instantiation arguments. This is slow.
    virtual CComPtr<IDiaSymbol> ResolveTemplateInstantiation( const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& ArgumentContainer ) = 0;

    // Returns public symbol information for symbol at given relative virtual address, or null if there is no public symbol there
    virtual const PublicSymbolInfo* FindPublicSymbolByRVA( DWORD RelativeVirtualAddress ) const = 0;
    // Attempts to find the parent type for this nested symbol. Returns nullptr if this is a top level symbol
    virtual CComPtr<IDiaSymbol> FindNestedTypeParent( const CComPtr<IDiaSymbol>& InNestedType ) = 0;

    // Returns the top level type for this symbol, or the symbol itself in case it is top level
    CComPtr<IDiaSymbol> FindTopLevelType( const CComPtr<IDiaSymbol>& InNestedType );
    // Attempts to remap type name
    virtual bool RemapTypeReference( const std::wstring& ClassNamespace, const std::wstring& ClassName, const TypeTemplateArgumentContainer& TypeArguments,
        std::wstring& OutClassReplacementNamespace, std::wstring& OutReplacementClassName, TypeTemplateArgumentContainer& OutReplacementTypeArguments ) const = 0;
    // Returns true if this template needs full definition of it's arguments, or just pre-declarations
    virtual bool DoTemplateArgumentsNeedFullDefinition( const SymbolNameInfo& TemplateName ) const = 0;
};

class TypeDependencyCollectorBase
{
protected:
    ITypeResolutionProvider* TypeResolver{};
    bool bNeedsCStdint{false};
public:
    explicit TypeDependencyCollectorBase( ITypeResolutionProvider* InTypeResolver );
    virtual ~TypeDependencyCollectorBase() = default;

    void CollectDependenciesForType( const CComPtr<IDiaSymbol>& InTypeSymbol, bool bOnlyWantsPredeclaration = false );
    void CollectDependenciesForUDTDefinition( const CComPtr<IDiaSymbol>& InUserDefinedType, bool bInternalMarkAsPredeclaration = false );
    void CollectDependenciesForTemplateInstantiation( const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments );
    FORCEINLINE bool NeedsCStdint() const { return bNeedsCStdint; }
    void AddDependency( const TypeDependency& TypeDependency, bool bIsPredeclaration );
protected:
    virtual void HandleTypedefDependency( const std::wstring& TypedefName, const CComPtr<IDiaSymbol>& TypedefType ) {}
    virtual void AddDependencyInternal( const TypeDependency& TypeDependency, bool bIsPredeclaration ) = 0;
    // true if template instnatiation was handled, false if slow template instantiation lookup through type resolution provider should take place
    virtual bool HandleTemplateInstantiationDependency( const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments ) { return false; }
};

class TypeDependencyCollector final : public TypeDependencyCollectorBase
{
    std::unordered_set<TypeDependency> DeclarationDependencies;
    std::unordered_set<TypeDependency> DefinitionDependencies;
    std::unordered_map<std::wstring, CComPtr<IDiaSymbol>> TypedefDependencies;
    std::unordered_set<DWORD> UserDefinedTypesHandled;
    bool bNeedsCStdint{false};
    bool bRecurseIntoUserDefinedTypes{false};
public:
    TypeDependencyCollector( ITypeResolutionProvider* InTypeResolver, bool InRecurseIntoUserDefinedTypes = false );

    FORCEINLINE const std::unordered_set<TypeDependency>& GetDeclarationDependencies() const { return DeclarationDependencies; }
    FORCEINLINE const std::unordered_set<TypeDependency>& GetDefinitionDependencies() const { return DefinitionDependencies; }

    std::unordered_set<TypeDependency> GetAllDependencies() const;
private:
    void HandleTypedefDependency(const std::wstring& TypedefName, const CComPtr<IDiaSymbol>& TypedefType) override;
    void AddDependencyInternal( const TypeDependency& TypeDependency, bool bIsPredeclaration ) override;
};
