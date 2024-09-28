#include <iostream>
#include <cassert>
#include <HeaderGenerator.h>
#include <HeaderGeneratorConfig.h>

#include "Utils/DiaUtils.h"
#include "Utils/StringUtils.h"

int main( int ArgC, const char** ArgV )
{
    if ( const HRESULT Result = CoInitializeEx( nullptr, COINIT_MULTITHREADED ); FAILED(Result) )
    {
        const _com_error ComError(Result);
        std::wcerr << L"COM Initialization failed: " << ComError.ErrorMessage() << std::endl;
        return 1;
    }

    CComPtr<IDiaDataSource> DataSource;
    if ( const HRESULT Result = CoCreateInstance( CLSID_DiaSource, nullptr, CLSCTX_INPROC_SERVER, __uuidof(IDiaDataSource), (void**) &DataSource ); FAILED(Result) )
    {
        const _com_error ComError(Result);
        std::wcout << L"Failed to create COM component for DIA SDK Source: " << ComError.ErrorMessage() << L". Make sure msdia140.dll is registered as a COM server." << std::endl;
        std::wcout << L"Attempting to fallback to loading msdia140.dll from the binary directory of the executable." << std::endl;
    }
    if ( DataSource == nullptr )
    {
        if (const HMODULE DiaSdkDLL = LoadLibraryW(L"msdia140.dll"); DiaSdkDLL == nullptr )
        {
            const _com_error ComError(HRESULT_FROM_WIN32(GetLastError()));
            std::wcout << L"Failed to load msdia140.dll from executable directory: " << ComError.ErrorMessage() << std::endl;
        }
        else if ( const HRESULT Result = CoCreateDiaDataSource( DiaSdkDLL, DataSource ); FAILED(Result) )
        {
            const _com_error ComError(Result);
            std::wcout << L"Failed to retrieve COM component for DIA SDK Source from DIA DLL: " << ComError.ErrorMessage() << std::endl;
        }
    }
    if ( DataSource == nullptr )
    {
        std::wcerr << L"Failed to load or find COM component for DIA SDK. Check output messages for more information" << std::endl;
        return 1;
    }

    if ( ArgC != 4 )
    {
        std::wcerr << L"Usage: " << ConvertMbStringToWide( ArgV[0] ) << L" Path/To/Executable.exe Path/To/Config.json Path/To/Output/Directory" << std::endl;
        return 2;
    }
    const std::filesystem::path ExecutableFilePath = ConvertMbStringToWide(ArgV[1]);
    const std::filesystem::path ConfigFilePath = ConvertMbStringToWide(ArgV[2]);
    const std::filesystem::path OutputDirectoryPath = ConvertMbStringToWide(ArgV[3]);

    if ( const HRESULT Result = DataSource->loadDataForExe( ExecutableFilePath.c_str(), ExecutableFilePath.parent_path().c_str(), nullptr ); Result != S_OK )
    {
        const _com_error ComError(Result);
        std::wcerr << L"Failed to load data from executable file at " << ExecutableFilePath.wstring() << L": " << ComError.ErrorMessage() << std::endl;
        return 1;
    }

    CComPtr<IDiaSession> DiaSession;
    if ( const HRESULT Result = DataSource->openSession( &DiaSession ); FAILED(Result) )
    {
        const _com_error ComError(Result);
        std::wcerr << L"Failed to create DIA SDK session: " << ComError.ErrorMessage() << std::endl;
        return 1;
    }
    assert( DiaSession->put_loadAddress(0) == S_OK );

    CComPtr<IDiaSymbol> GlobalExecutableSymbol;
    if ( const HRESULT Result = DiaSession->get_globalScope( &GlobalExecutableSymbol ); FAILED(Result) )
    {
        const _com_error ComError(Result);
        std::wcerr << L"Failed to retrieve SymTagExe global symbol from PDB data store: " << ComError.ErrorMessage() << std::endl;
        return 1;
    }

    HeaderGeneratorConfigManager ConfigManager;
    if ( !ConfigManager.LoadConfigFrom( ConfigFilePath ) )
    {
        std::wcerr << L"Failed to load config from " << ConfigFilePath.generic_wstring() << std::endl;
        return 1;
    }

    HeaderGenerator HeaderGenerator{ ExecutableFilePath.filename().wstring(), OutputDirectoryPath };
    HeaderGenerator.LoadDataFromConfig( ConfigManager.GetMergedConfig() );

    time_t StartTime = time(nullptr);
    HeaderGenerator.Generate( GlobalExecutableSymbol );
    time_t TotalTime = time(nullptr) - StartTime;
    std::cout << "Done with header generation in " << TotalTime << " seconds" << std::endl;
    return 0;
}
