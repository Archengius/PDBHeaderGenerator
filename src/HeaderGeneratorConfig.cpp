#include "HeaderGeneratorConfig.h"
#include <fstream>
#include <iostream>

bool HeaderGeneratorConfigManager::LoadConfigFrom(const std::filesystem::path& InConfigFilePath)
{
    const std::string ConfigBareName = InConfigFilePath.filename().replace_extension().generic_string();
    if ( LoadedConfigs.contains( ConfigBareName ) )
    {
        return true;
    }
    LoadedConfigs.insert( ConfigBareName );

    std::ifstream ConfigInputStream( InConfigFilePath );
    if ( !ConfigInputStream.good() )
    {
        std::cerr << "Failed to load configuration file from " << InConfigFilePath.generic_string() << " because file could not be open for read." << std::endl;
        return false;
    }
    HeaderGeneratorConfig LoadedConfig;
    try
    {
        const nlohmann::json ParsedJson = nlohmann::json::parse( ConfigInputStream );
        ParsedJson.get_to<HeaderGeneratorConfig>( LoadedConfig );
    }
    catch (const std::exception& ex)
    {
        std::cerr << "Failed to parse configuration file at " << InConfigFilePath.generic_string() << " as valid JSON: " << ex.what() << std::endl;
        return false;
    }

    // Attempt to load the dependencies first. We assume dependencies to be located in the same folder as this configuration file, and has the same extension
    for ( const std::string& DependencyConfigName : LoadedConfig.DependencyConfigs )
    {
        const std::filesystem::path DependencyConfigPath = InConfigFilePath.parent_path().operator/=(DependencyConfigName).replace_extension( InConfigFilePath.extension() );
        if ( !LoadConfigFrom( DependencyConfigPath ) )
        {
            std::cerr << "Refusing to load configuration file at " << InConfigFilePath.generic_string() << " because it's dependency config file " << DependencyConfigPath.generic_string() << " could not be loaded." << std::endl;
            return false;
        }
    }

    // Generate filesystem paths for header overrides in this config
    for ( ManualHeaderDefinition& HeaderDefinition : LoadedConfig.HeaderOverrides )
    {
        HeaderDefinition.HeaderFilesystemPath = InConfigFilePath.parent_path() / HeaderDefinition.HeaderPath;
    }

    // Merge the current config file with the global one, overwriting the dependencies or appending to them
    MergedConfig.Merge(LoadedConfig);
    return true;
}
