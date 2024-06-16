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
    }
    friend void from_json(const nlohmann::json& Json, HeaderGeneratorConfig& Config)
    {
        if (Json.contains("DependencyConfigs"))
        {
            Json.at("DependencyConfigs").get_to(Config.DependencyConfigs);
        }
        Json.at("ExternalHeaders").get_to(Config.ExternalHeaders);
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