#pragma once

#include <cstdint>
#include <string>
#include <unordered_set>
#include <nlohmann/json.hpp>

struct ExternalHeaderDefinition
{
    std::string IncludeName;
    std::vector<std::string> ContainedTypes;
    std::vector<std::string> ContainedNamespaces;

    friend void to_json(nlohmann::json& Json, const ExternalHeaderDefinition& HeaderDefinition)
    {
        Json["IncludeName"] = HeaderDefinition.IncludeName;
        Json["ContainedTypes"] = HeaderDefinition.ContainedTypes;
        Json["ContainedNamespaces"] = HeaderDefinition.ContainedNamespaces;
    }
    friend void from_json(const nlohmann::json& Json, ExternalHeaderDefinition& HeaderDefinition)
    {
        Json.at("IncludeName").get_to(HeaderDefinition.IncludeName);
        if (Json.contains("ContainedTypes"))
        {
            Json.at("ContainedTypes").get_to(HeaderDefinition.ContainedTypes);
        }
        if (Json.contains("ContainedNamespaces"))
        {
            Json.at("ContainedNamespaces").get_to(HeaderDefinition.ContainedNamespaces);
        }
    }
};

struct TypeRemapDefinition
{
    std::string OriginalTypeName;
    std::string ReplacementTypeName;

    NLOHMANN_DEFINE_TYPE_INTRUSIVE( TypeRemapDefinition, OriginalTypeName, ReplacementTypeName );
};

struct ManualHeaderDefinition
{
    std::string HeaderPath;
    std::string HeaderName;
    std::vector<std::string> ContainedTypes;
    // Transient, populated on config load
    std::filesystem::path HeaderFilesystemPath;

    NLOHMANN_DEFINE_TYPE_INTRUSIVE( ManualHeaderDefinition, HeaderPath, HeaderName, ContainedTypes );
};

struct HeaderGeneratorConfig
{
    std::vector<std::string> DependencyConfigs;
    std::vector<ExternalHeaderDefinition> ExternalHeaders;
    std::vector<std::string> ExternalGlobalData;
    std::vector<std::string> ExternalGlobalFunctions;
    std::unordered_map<std::string, std::string> GlobalDataNameRemap;
    std::vector<std::string> ExternalNamespaces;
    std::vector<std::string> ExternalTemplatePredeclarationWhitelist;
    std::vector<TypeRemapDefinition> TypeRemap;
    std::vector<ManualHeaderDefinition> HeaderOverrides;
    std::vector<std::string> LibrariesConsideredInternal;

    void Merge(const HeaderGeneratorConfig& OtherConfig)
    {
        ExternalGlobalData.insert(ExternalGlobalData.end(), OtherConfig.ExternalGlobalData.begin(), OtherConfig.ExternalGlobalData.end());
        ExternalGlobalFunctions.insert(ExternalGlobalFunctions.end(), OtherConfig.ExternalGlobalFunctions.begin(), OtherConfig.ExternalGlobalFunctions.end());
        ExternalNamespaces.insert(ExternalNamespaces.end(), OtherConfig.ExternalNamespaces.begin(), OtherConfig.ExternalNamespaces.end());
        TypeRemap.insert(TypeRemap.end(), OtherConfig.TypeRemap.begin(), OtherConfig.TypeRemap.end());
        ExternalTemplatePredeclarationWhitelist.insert(ExternalTemplatePredeclarationWhitelist.end(), OtherConfig.ExternalTemplatePredeclarationWhitelist.begin(), OtherConfig.ExternalTemplatePredeclarationWhitelist.end());
        HeaderOverrides.insert(HeaderOverrides.end(), OtherConfig.HeaderOverrides.begin(), OtherConfig.HeaderOverrides.end());
        LibrariesConsideredInternal.insert(LibrariesConsideredInternal.end(), OtherConfig.LibrariesConsideredInternal.begin(), OtherConfig.LibrariesConsideredInternal.end());

        for ( const auto& [OriginalName, RemappedName] : OtherConfig.GlobalDataNameRemap )
        {
            GlobalDataNameRemap.insert_or_assign( OriginalName, RemappedName );
        }

        std::unordered_map<std::string, int32_t> ExistingHeaderIndices;
        for ( int32_t i = 0; i < ExternalHeaders.size(); i++ )
        {
            ExistingHeaderIndices.insert({ ExternalHeaders[i].IncludeName, i });
        }

        for ( const ExternalHeaderDefinition& HeaderDefinition : OtherConfig.ExternalHeaders )
        {
            if ( const auto Iterator = ExistingHeaderIndices.find( HeaderDefinition.IncludeName ); Iterator != ExistingHeaderIndices.end() )
            {
                ExternalHeaderDefinition& ExistingDefinition = ExternalHeaders[ Iterator->second ];
                ExistingDefinition.ContainedNamespaces.insert( ExistingDefinition.ContainedNamespaces.end(), HeaderDefinition.ContainedNamespaces.begin(), HeaderDefinition.ContainedNamespaces.end() );
                ExistingDefinition.ContainedTypes.insert( ExistingDefinition.ContainedTypes.end(), HeaderDefinition.ContainedTypes.begin(), HeaderDefinition.ContainedTypes.end() );
            }
            else
            {
                ExternalHeaders.push_back( HeaderDefinition );
            }
        }
    }

    friend void to_json(nlohmann::json& Json, const HeaderGeneratorConfig& Config)
    {
        Json["DependencyConfigs"] = Config.DependencyConfigs;
        Json["ExternalHeaders"] = Config.ExternalHeaders;
        Json["ExternalGlobalData"] = Config.ExternalGlobalData;
        Json["ExternalGlobalFunctions"] = Config.ExternalGlobalFunctions;
        Json["GlobalDataNameRemap"] = Config.GlobalDataNameRemap;
        Json["ExternalNamespaces"] = Config.ExternalNamespaces;
        Json["ExternalTemplatePredeclarationWhitelist"] = Config.ExternalTemplatePredeclarationWhitelist;
        Json["HeaderOverrides"] = Config.HeaderOverrides;
        Json["TypeRemap"] = Config.TypeRemap;
        Json["LibrariesConsideredInternal"] = Config.LibrariesConsideredInternal;
    }
    friend void from_json(const nlohmann::json& Json, HeaderGeneratorConfig& Config)
    {
        if (Json.contains("DependencyConfigs"))
        {
            Json.at("DependencyConfigs").get_to(Config.DependencyConfigs);
        }
        if (Json.contains("ExternalHeaders"))
        {
            Json.at("ExternalHeaders").get_to(Config.ExternalHeaders);
        }
        if (Json.contains("ExternalGlobalData"))
        {
            Json.at("ExternalGlobalData").get_to(Config.ExternalGlobalData);
        }
        if (Json.contains("ExternalGlobalFunctions"))
        {
            Json.at("ExternalGlobalFunctions").get_to(Config.ExternalGlobalFunctions);
        }
        if (Json.contains("GlobalDataNameRemap"))
        {
            Json.at("GlobalDataNameRemap").get_to(Config.GlobalDataNameRemap);
        }
        if (Json.contains("ExternalNamespaces"))
        {
            Json.at("ExternalNamespaces").get_to(Config.ExternalNamespaces);
        }
        if (Json.contains("ExternalTemplatePredeclarationWhitelist"))
        {
            Json.at("ExternalTemplatePredeclarationWhitelist").get_to(Config.ExternalTemplatePredeclarationWhitelist);
        }
        if (Json.contains("HeaderOverrides"))
        {
            Json.at("HeaderOverrides").get_to(Config.HeaderOverrides);
        }
        if (Json.contains("TypeRemap"))
        {
            Json.at("TypeRemap").get_to(Config.TypeRemap);
        }
        if (Json.contains("LibrariesConsideredInternal"))
        {
            Json.at("LibrariesConsideredInternal").get_to(Config.LibrariesConsideredInternal);
        }
    }
};

class HeaderGeneratorConfigManager
{
    std::unordered_set<std::string> LoadedConfigs;
    HeaderGeneratorConfig MergedConfig;
public:
    bool LoadConfigFrom( const std::filesystem::path& InConfigFilePath );
    const HeaderGeneratorConfig& GetMergedConfig() const { return MergedConfig; }
};