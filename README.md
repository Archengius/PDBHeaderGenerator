1. make sure you have Visual Studio 2022 installed
2. build cmake project
3. navigate to `Microsoft Visual Studio\2022\Community\DIA SDK\bin\amd64` and run command in elevated visual studio developer console: `regsvr32 msdia140.dll`
4. example command to run: `PDBHeaderGenerator.exe "Path/To/Executable.exe" "Path/To/Config.json" "Path/To/Output/Directory"`

configs in `resources/`, use `config-libraries.json` as the base and add new configs as dependencies to it