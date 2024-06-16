#pragma once

#include <cstdint>
#include <memory>
#include <vector>
#include <xstring>
#include "TypeDependencyCollector.h"
#include "Utils/DiaUtils.h"

class IUDTDeclarationMember;
struct TypeTemplateArgument;
class GlobalFunctionDeclaration;
class GlobalDataDeclaration;
struct UDTDeclarationData;
class EnumDeclaration;
class EnumerationInfo;
class ITopLevelDeclaration;
class HeaderGenerator;
class UserDefinedTypeInfo;
struct EnumDeclarationData;
class CppFile;

class GeneratedHeaderFile
{
    HeaderGenerator* OwnerGenerator{};
    std::wstring HeaderFileName;
    std::unique_ptr<CppFile> GeneratedFile;

    std::vector<UserDefinedTypeInfo*> ContainedTopLevelTypes;
    std::vector<EnumerationInfo*> ContainedTopLevelEnums;
    std::vector<CComPtr<IDiaSymbol>> ContainedGlobalFunctions;
    std::vector<CComPtr<IDiaSymbol>> ContainedGlobalVariables;
public:
    GeneratedHeaderFile( HeaderGenerator* InOwnerGenerator, const std::wstring& InHeaderFileName );

    FORCEINLINE HeaderGenerator* GetHeaderGenerator() const { return OwnerGenerator; }
    FORCEINLINE const std::wstring& GetHeaderFileName() const { return HeaderFileName; }

    void AddTopLevelType( UserDefinedTypeInfo* InUserDefinedType );
    void AddTopLevelEnumeration( EnumerationInfo* InEnumerationInfo );
    void AddGlobalFunction( const CComPtr<IDiaSymbol>& InGlobalFunction );
    void AddGlobalVariable( const CComPtr<IDiaSymbol>& InGlobalVariable );
    void GenerateCppFile();
private:
    void PopulateCppFile() const;
public:
    static std::shared_ptr<GlobalDataDeclaration> MakeTopLevelData( const CComPtr<IDiaSymbol>& InDataSymbol, ITypeResolutionProvider* TypeProvider );
    static std::shared_ptr<GlobalFunctionDeclaration> MakeTopLevelFunction( const CComPtr<IDiaSymbol>& InFunctionSymbol, ITypeResolutionProvider* TypeProvider );
    static std::shared_ptr<EnumDeclarationData> MakeEnum( const CComPtr<IDiaSymbol>& EnumTypeSymbol, std::wstring* OutEnumNamespace, ITypeResolutionProvider* TypeProvider );
    static std::shared_ptr<UDTDeclarationData> MakeUDT( const CComPtr<IDiaSymbol>& UDTSymbol, const UserDefinedTypeInfo* TypeInfo, std::wstring* OutTypeNamespace, ITypeResolutionProvider* TypeProvider );
    static std::shared_ptr<ITopLevelDeclaration> MakePredeclaration( const CComPtr<IDiaSymbol>& InEnumSymbol, ITypeResolutionProvider* TypeProvider );
    static std::shared_ptr<ITopLevelDeclaration> MakeTemplateDeclaration( const SymbolNameInfo& TemplateInstantiationName, const CComPtr<IDiaSymbol>& TemplateInstantiationSymbol, ITypeResolutionProvider* TypeProvider );
};

class HeaderFileReferenceCollector final : TypeDependencyCollectorBase
{
    const GeneratedHeaderFile* OwnerHeader{};
    std::unordered_map<DWORD, ITopLevelDeclaration*> TopLevelDeclarations;
    std::unordered_set<std::wstring> LocalHeaderIncludes;
    std::unordered_set<std::wstring> SystemHeaderIncludes;
    ITopLevelDeclaration* CurrentlyCollectingDeclaration{};
    std::unordered_set<DWORD> DependenciesAlreadyHandled;
    std::unordered_map<DWORD, ITopLevelDeclaration*> TypeToPredeclarationLookup;
    std::unordered_map<std::wstring, ITopLevelDeclaration*> TemplateToPredeclarationLookup;
    std::vector<std::shared_ptr<ITopLevelDeclaration>> AllPredeclarations;
public:
    explicit HeaderFileReferenceCollector( const GeneratedHeaderFile* InOwnerHeader );

    void PopulateWithTopLevelDefinitions( const std::unordered_map<IDiaSymbol*, ITopLevelDeclaration*>& InRawDefinitionMap );
    void PopulateGeneratedFileWithDependencies( CppFile& CppFile ) const;
    void CollectDependenciesForTypeAndNestedTypes( const UserDefinedTypeInfo* InTypeInfo );
protected:
    ITopLevelDeclaration* FindOrCreateTemplateDeclaration( const SymbolNameInfo& TemplateInstantiationName, const CComPtr<IDiaSymbol>& TemplateInstantiationSymbol );
    bool HandleTemplateInstantiationDependency(const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments) override;
    void HandleTypedefDependency(const std::wstring& TypedefName, const CComPtr<IDiaSymbol>& TypedefType) override;
    void AddDependencyInternal(const TypeDependency& TypeDependency, bool bIsPredeclaration) override;
};

class NestedTypeDependencyCollector final : TypeDependencyCollectorBase
{
    std::unordered_map<DWORD, IUDTDeclarationMember*> NestedTypeDeclarations;
    IUDTDeclarationMember* CurrentlyCollectingDeclaration{};
    std::unordered_set<DWORD> DependenciesAlreadyHandled;
public:
    explicit NestedTypeDependencyCollector( ITypeResolutionProvider* InTypeProvider );

    void PopulateWithNestedDeclarations( const std::unordered_map<DWORD, IUDTDeclarationMember*>& InNestedTypeDeclarations );
    void CollectDependenciesForNestedType( const UserDefinedTypeInfo* InTypeInfo );
private:
    void CollectDependenciesForTypeAndNestedTypes( const UserDefinedTypeInfo* InTypeInfo );
    bool HandleTemplateInstantiationDependency(const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments) override;
    void AddDependencyInternal(const TypeDependency& TypeDependency, bool bIsPredeclaration) override;
};
