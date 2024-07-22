cmake_minimum_required(VERSION 3.28)
project(PDBHeaderGenerator)

set(CMAKE_CXX_STANDARD 20)

file(GLOB_RECURSE SOURCE_FILES src/*.cpp )
string(TOLOWER ${CMAKE_SYSTEM_PROCESSOR} SYSTEM_PROCESSOR_LOWERCASE)

add_executable(PDBHeaderGenerator ${SOURCE_FILES} )
target_include_directories(PDBHeaderGenerator PUBLIC include)

include(FetchContent)

set(VSINSTALLDIR $ENV{VSINSTALLDIR})
if(NOT VSINSTALLDIR)
    message(FATAL_ERROR "VSINSTALLDIR environment variable is not set. Please ensure Visual Studio is installed and the environment variable is configured.")
endif()

set(DIA_SDK_INCLUDE_DIR "${VSINSTALLDIR}/DIA SDK/include")
set(DIA_SDK_LIB_DIR "${VSINSTALLDIR}/DIA SDK/lib/${SYSTEM_PROCESSOR_LOWERCASE}")
set(DIA_SDK_BIN_DIR "${VSINSTALLDIR}/DIA SDK/bin/${SYSTEM_PROCESSOR_LOWERCASE}")

target_include_directories(PDBHeaderGenerator PRIVATE ${DIA_SDK_INCLUDE_DIR})
target_link_libraries(PDBHeaderGenerator PRIVATE ${DIA_SDK_LIB_DIR}/diaguids.lib)

set_target_properties(PDBHeaderGenerator PROPERTIES
        VS_DEBUGGER_ENVIRONMENT "PATH=%PATH%;${DIA_SDK_BIN_DIR}"
)

FetchContent_Declare(
        nlohmann_json
        GIT_REPOSITORY https://github.com/nlohmann/json.git
        GIT_TAG v3.11.3
)

FetchContent_MakeAvailable(nlohmann_json)
target_link_libraries(PDBHeaderGenerator PRIVATE nlohmann_json::nlohmann_json)