#pragma once

#include <cstdint>
#include <filesystem>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <AST/CppTypeDeclaration.h>

#include "Utils/DiaUtils.h"
#include "TypeDependencyCollector.h"
#include "Utils/StringUtils.h"

struct HeaderGeneratorConfig;
struct TypeTemplateArgument;
enum class CppUDTKind : uint8_t;
struct UDTDeclarationData;
enum class CppAccessModifier : uint8_t;
class UDTDeclaration;
class EnumDeclaration;
class ITopLevelDeclaration;
class CppFile;
class HeaderGenerator;
class EnumerationInfo;

class CompilationUnit
{
    HeaderGenerator* OwnerGenerator;
    CComPtr<IDiaSymbol> CompilationUnitSymbol;
    std::wstring CompilationUnitName;
    // environment in which the compiland was compiled. Useful for checking potential used includes, defines and source filename
    std::unordered_map<std::wstring, std::wstring> CompilandEnvironment;
    DWORD CompilandPlatform{};
    DWORD CompilandSourceLanguage{};

    // True if this object file comes from another statically of dynamically linked library
    bool bIsExternalCompilationUnit{false};
public:
    // Global functions defined in this object file
    std::vector<CComPtr<IDiaSymbol>> GlobalFunctions;
    std::vector<CComPtr<IDiaSymbol>> GlobalVariables;

    std::vector<CComPtr<IDiaSymbol>> AllFunctionImplementations;
    std::vector<CComPtr<IDiaSymbol>> AllDataMemberDefinitions;
    bool bIsGlobalScopeUnit{false};
public:
    CompilationUnit( HeaderGenerator* InOwnerGenerator, const CComPtr<IDiaSymbol>& InCompilationUnitSymbol, const std::wstring& InCompilationUnitName, bool InIsExternalCompilationUnit );

    FORCEINLINE const std::wstring& GetCompilationUnitName() const { return CompilationUnitName; }
    FORCEINLINE bool IsExternalCompilationUnit() const { return bIsExternalCompilationUnit; }
    FORCEINLINE HeaderGenerator* GetHeaderGenerator() const { return OwnerGenerator; }

    int32_t EstimateActionCount() const;
    void ProcessActions_Pass1( int32_t& CurrentActionNumber );
    void ProcessActions_Pass2( int32_t& CurrentActionNumber );
    std::wstring GetHeaderFilename() const;
private:
    void ProcessCompilandEnvironment();
    void ProcessCompilandDetails();
    void ProcessCompilandFunctionsAndData( int32_t& CurrentActionNumber );

    bool NeedsHeaderForCompilationUnit() const;
    void PushGlobalMembersToHeaderFile( class GeneratedHeaderFile* HeaderFile ) const;
    void CollectAndRegisterDependencies( int32_t& CurrentActionNumber );
    static void SanitizeGlobalMemberNameForHeaderFilename( std::wstring& SymbolName );
};

class CompilationUnitReferenceCollector final : public TypeDependencyCollectorBase
{
    CompilationUnit* mCompilationUnit{nullptr};
    std::unordered_set<DWORD> VisitedTypes;
public:
    explicit CompilationUnitReferenceCollector( CompilationUnit* InCompilationUnit );
private:
    void AddDependencyInternal(const TypeDependency& TypeDependency, bool bIsPredeclaration) override;
    bool HandleTemplateInstantiationDependency(const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& TemplateArguments) override;
};

class UserDefinedTypeInfo
{
    HeaderGenerator* OwnerGenerator{};
    CComPtr<IDiaSymbol> UserDefinedTypeSymbol;
    std::vector<UserDefinedTypeInfo*> NestedTypes;
    std::vector<EnumerationInfo*> NestedEnums;
    std::unordered_map<const CompilationUnit*, std::vector<CComPtr<IDiaSymbol>>> CompilationUnitDefinitions;
    std::unordered_set<const CompilationUnit*> CompilationUnitReferences;
    SymbolNameInfo SymbolName;
public:
    UserDefinedTypeInfo( HeaderGenerator* InGenerator, const CComPtr<IDiaSymbol>& InUDTSymbol );

    FORCEINLINE const SymbolNameInfo& GetSymbolName() const { return SymbolName; }
    FORCEINLINE CComPtr<IDiaSymbol> GetUDTSymbol() const { return UserDefinedTypeSymbol; }
    FORCEINLINE const std::vector<UserDefinedTypeInfo*>& GetNestedTypes() const { return NestedTypes; }
    FORCEINLINE const std::vector<EnumerationInfo*>& GetNestedEnums() const { return NestedEnums; }

    void ProcessActions();
    void AddDefinedFunction( const CComPtr<IDiaSymbol>& FunctionSymbol, const CompilationUnit* CompilationUnit );
    void AddDefinedVariable( const CComPtr<IDiaSymbol>& FieldSymbol, const CompilationUnit* CompilationUnit );
    void AddNestedUserDefinedType( UserDefinedTypeInfo* InNestedUDT );
    void AddNestedEnumeration( EnumerationInfo* InNestedEnumeration );
    // Returns true if the given compilation unit should recursively scan dependencies from this type. That would be the case if this unit is the unit that implements this type, or the first unit to reference the type.
    bool AddCompilationUnitReference( const CompilationUnit* FromCompilationUnit );
private:
    std::wstring GetHeaderNameForType() const;
};

class EnumerationInfo
{
    HeaderGenerator* OwnerGenerator{};
    CComPtr<IDiaSymbol> EnumerationSymbol;
    std::unordered_set<const CompilationUnit*> CompilationUnitReferences;
public:
    EnumerationInfo( HeaderGenerator* InGenerator, const CComPtr<IDiaSymbol>& InEnumSymbol );

    FORCEINLINE CComPtr<IDiaSymbol> GetEnumSymbol() const { return EnumerationSymbol; }

    void ProcessActions();
    void AddCompilationUnitReference( const CompilationUnit* FromCompilationUnit );
private:
    std::wstring DetermineHeaderFilename() const;
};

using TemplateInstantiationMap = std::unordered_map<TypeTemplateArgumentContainer, CComPtr<IDiaSymbol>>;

enum class EExternalSymbolType : uint8_t
{
    None = 0x00,
    Type = 0x01,
    Function = 0x02,
    Data = 0x04
};
inline EExternalSymbolType operator|(EExternalSymbolType A, EExternalSymbolType B)
{
    return static_cast<EExternalSymbolType>(static_cast<uint8_t>(A) | static_cast<uint8_t>(B));
}
inline EExternalSymbolType operator&(EExternalSymbolType A, EExternalSymbolType B)
{
    return static_cast<EExternalSymbolType>(static_cast<uint8_t>(A) & static_cast<uint8_t>(B));
}

struct TypeSubstitutionCandidate
{
    TypeTemplateArgumentContainer FilterTemplateArguments;
    std::wstring SubtitutedNamespace;
    std::wstring SubtitutedClassName;
    TypeTemplateArgumentContainer SubstitutedArguments;
};

class HeaderGenerator final : public ITypeResolutionProvider
{
    std::filesystem::path OutputDirectoryPath;
    std::unordered_map<DWORD, UserDefinedTypeInfo*> UserDefinedTypesLookup;
    std::vector<std::shared_ptr<UserDefinedTypeInfo>> AllUserDefinedTypes;
    std::unordered_map<DWORD, EnumerationInfo*> EnumLookup;
    std::vector<std::shared_ptr<EnumerationInfo>> AllEnums;
    std::unordered_map<DWORD, GeneratedHeaderFile*> SymbolHeaderFiles;
    std::unordered_map<std::wstring, GeneratedHeaderFile*> GeneratedHeaderFilesLookup;
    std::vector<std::shared_ptr<GeneratedHeaderFile>> AllGeneratedHeaderFiles;
    std::unordered_set<DWORD> ExternalUserDefinedTypes;
    std::unordered_set<std::wstring> ExternalTemplates;
    int32_t TotalActionCount{0};
    CComPtr<IDiaSymbol> ExecutableSymbol;
    // all symbols defined across all compilation units by their hash. used to detect duplicate symbol definitions and handle them accordingly.
    std::unordered_set<size_t> AllDefinedSymbolNameHashes;
    // symbols that have been defined in multiple translation units with the same name. Usually that means they were marked as "static" or "inline", or are template instantiations
    std::unordered_set<std::wstring> DuplicateSymbolDefinitions;
    // Map of external types to the names of the headers where they can be found
    std::unordered_map<std::wstring, std::wstring> ExternalTypeToHeaderLookup;
    std::unordered_set<std::wstring> ExternalFunctions;
    std::unordered_set<std::wstring> ExternalData;
    std::unordered_set<std::wstring> ExternalNamespaces;
    std::unordered_set<std::wstring> ExternalTemplatePredeclarationWhitelist;
    std::unordered_map<std::pair<std::wstring, std::wstring>, std::vector<TypeSubstitutionCandidate>> TypeSubstitutions;
    std::unordered_map<std::wstring, std::filesystem::path> HeaderOverrideNameToFilePath;
    std::unordered_map<std::wstring, std::wstring> HeaderOverridenTypeNameToHeaderName;
    // Fallback lookup of external namespace (starting with) to the header. Used to handle libraries with many, many different types where having different types is difficult, such as highly-templated libraries.
    std::vector<std::pair<std::wstring, std::wstring>> ExternalNamespaceToHeaderFallback;
    std::unordered_map<std::wstring, CComPtr<IDiaSymbol>> EnumerationByNameCache;
    std::unordered_map<std::wstring, CComPtr<IDiaSymbol>> UDTsByNameCache;
    std::unordered_map<std::wstring, std::shared_ptr<TemplateInstantiationMap>> TemplatesByNameCache;
    std::unordered_map<DWORD, PublicSymbolInfo> PublicSymbolsByRVA;
    std::unordered_set<DWORD> ExternalFunctionsAndDataRVAs;
    // Cache of nested type ID to the parent type symbol
    std::unordered_map<DWORD, CComPtr<IDiaSymbol>> NestedTypeParentCache;
    // Cache of lexical parent to it's owner compilation unit
    std::vector<std::shared_ptr<CompilationUnit>> AllCompilationUnits;
    std::unordered_set<std::wstring> LibrariesConsideredInternal;
    std::unordered_set<std::wstring> AlreadyPrintedLibraryNames;
    std::wstring DllName;
public:
    explicit HeaderGenerator( const std::wstring& InDllName, const std::filesystem::path& InOutputDirectory );
    void LoadDataFromConfig( const HeaderGeneratorConfig& Config );

    void Generate( const CComPtr<IDiaSymbol>& InGlobalSymbol );

    FORCEINLINE std::filesystem::path GetOutputDirectory() const { return OutputDirectoryPath; }
    FORCEINLINE CComPtr<IDiaSymbol> GetExecutableSymbol() const { return ExecutableSymbol; }
    FORCEINLINE int32_t GetTotalActionCount() const { return TotalActionCount; }
    UserDefinedTypeInfo* FindOrAddUserDefinedType( const CComPtr<IDiaSymbol>& ClassSymbol );
    EnumerationInfo* FindOrAddEnum( const CComPtr<IDiaSymbol>& EnumSymbol );
    GeneratedHeaderFile* FindOrCreateHeaderFile( const std::wstring& HeaderFilename );
    UserDefinedTypeInfo* FindUserDefinedType( const CComPtr<IDiaSymbol>& ClassSymbol ) const;

    // Attempts to resolve the type by full name, which might contain a namespace or another type as a scope. Does not handle enumerations nested into templated types!!!!
    CComPtr<IDiaSymbol> ResolveEnumByFullName( const std::wstring& InFullEnumName ) override;
    CComPtr<IDiaSymbol> ResolveUDTByFullName( const std::wstring& InFullClassName ) override;
    // Attempts to resolve the type for a specific template instantiation given the template full name and instantiation arguments. This is slow.
    CComPtr<IDiaSymbol> ResolveTemplateInstantiation( const SymbolNameInfo& TemplateName, const TypeTemplateArgumentContainer& ArgumentContainer ) override;

    bool IsExternalUserDefinedType( const CComPtr<IDiaSymbol>& InTypeSymbol ) const;
    // Returns true if this type has not been registered as external before
    bool RegisterExternalUserDefinedType( const CComPtr<IDiaSymbol>& InExternalSymbol );
    // Registers template as external
    void RegisterExternalTemplate( const SymbolNameInfo& TemplateName );
    // Registers external symbol by it's RVA. Should only be used on functions and data. Will do nothing if symbol location is not LocIsStatic
    void RegisterExternalSymbolByRVA( const CComPtr<IDiaSymbol>& InSymbol );

    // Returns public symbol information for symbol at given relative virtual address, or null if there is no public symbol there
    const PublicSymbolInfo* FindPublicSymbolByRVA( DWORD RelativeVirtualAddress ) const override;
    CComPtr<IDiaSymbol> FindNestedTypeParent(const CComPtr<IDiaSymbol>& InNestedType) override;
    bool RemapTypeReference(const std::wstring& ClassNamespace, const std::wstring& ClassName, const TypeTemplateArgumentContainer& TypeArguments, std::wstring& OutClassReplacementNamespace, std::wstring& OutReplacementClassName, TypeTemplateArgumentContainer& OutReplacementTypeArguments) const override;
    bool DoTemplateArgumentsNeedFullDefinition(const SymbolNameInfo& TemplateName) const override;
    // Returns true if symbol at the given RVA (function or data) is marked as external, e.g. has it's definition in one of the external object files (part of either statically or dynamically linked library)
    bool IsExternalSymbolByRVA( DWORD RelativeVirtualAddress ) const;

    void PushDefinedSymbol( const std::wstring& InFullSymbolName );
    bool IsDuplicateSymbolDefinition( const std::wstring& InFullSymbolName ) const;

    // Returns true if the given symbol should be marked as external as defined by the user
    bool ShouldMarkSymbolAsExternal( const SymbolNameInfo& SymbolName, EExternalSymbolType SymbolType ) const;
    const std::wstring* FindExternalHeaderForType( const SymbolNameInfo& SymbolName ) const;

    bool IsHeaderFilenameOccupiedByManualHeader( const std::wstring& InHeaderFilename ) const;
    const std::wstring* FindOverridenManualHeaderForType( const SymbolNameInfo& ClassName ) const;

    // Finds the header file that has been associated with the given symbol (either UDT or an enum). Also works for global functions and data
    GeneratedHeaderFile* FindHeaderFileForSymbol( const CComPtr<IDiaSymbol>& InSymbol );
    void AssociateSymbolWithHeaderFile( const CComPtr<IDiaSymbol>& InSymbol, GeneratedHeaderFile* InHeaderFile );

    static bool IsFunctionGeneratedThunk( const CComPtr<IDiaSymbol>& FunctionSymbol );
private:
    std::shared_ptr<TemplateInstantiationMap> FindAllTemplateInstantiationSymbols( const SymbolNameInfo& TemplateName );
    CComPtr<IDiaSymbol> ResolveSymbolByFullNameNoCache( const std::wstring& InFullSymbolName, enum SymTagEnum InSymbolTag ) const;
    bool ResolveScopeForFullName( const std::wstring& InFullName, CComPtr<IDiaSymbol>& OutScopeSymbol, std::wstring& OutNameInsideScope ) const;
    std::shared_ptr<CompilationUnit> CreateCompilationUnitForCompiland( const CComPtr<IDiaSymbol>& CompilandSymbol );
    void CreateShortImportLibrary();
    void DiscoverAllSymbols( int32_t& CurrentActionNumber );
    void DiscoverAllCompilands();
};
