#pragma once

#include <cstdint>
#include "Utils/MemoryGrowableBuffer.h"

class COFFImportLibrary
{
    std::string LibraryNameWithExtension;
    uint64_t TargetMachine{};
    std::vector<std::pair<std::string, MemoryGrowableBuffer>> Members;
    std::string LibraryName;
    std::string ImportDescriptorSymbolName;
    std::string NullThunkSymbolName;
    uint64_t CurrentOrdinalIndex{};
public:
    COFFImportLibrary( const std::string& InLibraryNameWithExtension, uint64_t InTargetMachine );

    void AddImport( const std::string& InImportName, bool bIsCodeImport );
    bool WriteLibrary( const std::wstring& InFilename );
private:
    void DiscoverSymbols( std::vector<std::pair<std::string, uint64_t>>& OutSymbolToMemberIndex ) const;

    std::pair<std::string, MemoryGrowableBuffer> CreateImportDescriptor() const;
    std::pair<std::string, MemoryGrowableBuffer> CreateNullImportDescriptor() const;
    std::pair<std::string, MemoryGrowableBuffer> CreateNullThunk() const;
};
