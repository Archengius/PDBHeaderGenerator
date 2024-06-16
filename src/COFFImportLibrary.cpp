#include "COFFImportLibrary.h"

#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <cassert>
#include <ranges>
#include <unordered_map>
#include <Windows.h>

const std::string_view ImportDescriptorPrefix = "__IMPORT_DESCRIPTOR_";
const std::string NullImportDescriptorSymbolName = "__NULL_IMPORT_DESCRIPTOR";
const std::string_view NullThunkDataPrefix = "\x7f";
const std::string_view NullThunkDataSuffix = "_NULL_THUNK_DATA";

static uint16_t GetImgRelRelocationForMachine(const DWORD Machine)
{
    switch (Machine)
    {
        case IMAGE_FILE_MACHINE_AMD64:
            return IMAGE_REL_AMD64_ADDR32NB;
        case IMAGE_FILE_MACHINE_ARM64:
            return IMAGE_REL_ARM64_ADDR32NB;
        default:
            assert(!"unsupported machine");
            return 0;
    }
}

COFFImportLibrary::COFFImportLibrary(const std::string& InLibraryNameWithExtension, const uint64_t InTargetMachine) : LibraryNameWithExtension(InLibraryNameWithExtension), TargetMachine(InTargetMachine)
{
    // Format a library name without extension
    LibraryName = LibraryNameWithExtension;
    if (const size_t ExtensionIndex = LibraryName.rfind(L'.'); ExtensionIndex != std::string::npos)
    {
        LibraryName.erase(ExtensionIndex);
    }

    // Build import descriptor name
    ImportDescriptorSymbolName.append(ImportDescriptorPrefix);
    ImportDescriptorSymbolName.append(LibraryName);

    // Build null thunk name
    NullThunkSymbolName.append(NullThunkDataPrefix);
    NullThunkSymbolName.append(LibraryName);
    NullThunkSymbolName.append(NullThunkDataSuffix);

    // Build initial members
    Members.push_back( CreateImportDescriptor() );
    Members.push_back( CreateNullImportDescriptor() );
    Members.push_back( CreateNullThunk() );
}

void COFFImportLibrary::AddImport(const std::string& InImportName, const bool bIsCodeImport)
{
    MemoryGrowableBuffer ResultObjectFileBuffer;

    IMPORT_OBJECT_HEADER ImportObjectHeader{};
    ImportObjectHeader.Sig1 = IMAGE_FILE_MACHINE_UNKNOWN;
    ImportObjectHeader.Sig2 = IMPORT_OBJECT_HDR_SIG2;
    ImportObjectHeader.Machine = TargetMachine;
    ImportObjectHeader.TimeDateStamp = static_cast<DWORD>(time(nullptr));
    ImportObjectHeader.NameType = IMPORT_OBJECT_NAME;

    assert( CurrentOrdinalIndex < 65535 && L"Ordinal overflow. COFF files do not support more than 65k imports/exports." );
    ImportObjectHeader.Ordinal = CurrentOrdinalIndex++;
    ImportObjectHeader.Type = bIsCodeImport ? IMPORT_OBJECT_CODE : IMPORT_OBJECT_DATA;
    ImportObjectHeader.SizeOfData = (InImportName.size() + 1) + (LibraryNameWithExtension.size() + 1);

    ResultObjectFileBuffer.Append(&ImportObjectHeader, sizeof(ImportObjectHeader));
    ResultObjectFileBuffer.Append(InImportName.c_str(), InImportName.size() + 1);
    ResultObjectFileBuffer.Append(LibraryNameWithExtension.c_str(), LibraryNameWithExtension.size() + 1);

    Members.push_back( std::pair( LibraryNameWithExtension, std::move( ResultObjectFileBuffer ) ) );
}

// Copied from LLVM mostly, with minor alternations to not depend on LLVM code
std::pair<std::string, MemoryGrowableBuffer> COFFImportLibrary::CreateImportDescriptor() const
{
    const uint32_t NumberOfSections = 2;
    const uint32_t NumberOfSymbols = 7;
    const uint32_t NumberOfRelocations = 3;

    MemoryGrowableBuffer ResultObjectFileBuffer;

    IMAGE_FILE_HEADER COFFFileHeader{};
    COFFFileHeader.Machine = TargetMachine;
    COFFFileHeader.NumberOfSections = NumberOfSections;
    COFFFileHeader.TimeDateStamp = 0;
    COFFFileHeader.PointerToSymbolTable = sizeof(COFFFileHeader) + (NumberOfSections * sizeof(IMAGE_SECTION_HEADER)) +
                                          // .idata$2
                                          sizeof(IMAGE_IMPORT_DESCRIPTOR) + NumberOfRelocations * sizeof(IMAGE_RELOCATION) +
                                          // .idata$4
                                          (LibraryNameWithExtension.size() + 1);
    COFFFileHeader.NumberOfSymbols = NumberOfSymbols;
    COFFFileHeader.SizeOfOptionalHeader = 0;
    COFFFileHeader.Characteristics = 0;

    ResultObjectFileBuffer.Append(&COFFFileHeader, sizeof(COFFFileHeader));

    // Initialize section table
    IMAGE_SECTION_HEADER SectionTable[NumberOfSections] = {};

    // .idata$2. $2 means that the contribution of this section will be to section .idata after .idata$1 and before .idata$3 and others
    const BYTE SectionNameIdata2[]{'.', 'i', 'd', 'a', 't', 'a', '$', '2'};
    memcpy(SectionTable[0].Name, SectionNameIdata2, sizeof(SectionNameIdata2));
    SectionTable[0].Misc.VirtualSize = 0;
    SectionTable[0].VirtualAddress = 0;
    SectionTable[0].SizeOfRawData = sizeof(IMAGE_IMPORT_DESCRIPTOR);
    SectionTable[0].PointerToRawData = sizeof(IMAGE_FILE_HEADER) + NumberOfSections * sizeof(IMAGE_SECTION_HEADER);
    SectionTable[0].PointerToRelocations = sizeof(IMAGE_FILE_HEADER) + NumberOfSections * sizeof(IMAGE_SECTION_HEADER) +
                                           sizeof(IMAGE_IMPORT_DESCRIPTOR);
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].NumberOfRelocations = NumberOfRelocations;
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].Characteristics =
            IMAGE_SCN_ALIGN_4BYTES | IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;

    // .idata$6
    const BYTE SectionNameIdata6[]{'.', 'i', 'd', 'a', 't', 'a', '$', '6'};
    memcpy(SectionTable[1].Name, SectionNameIdata6, sizeof(SectionNameIdata6));
    SectionTable[1].Misc.VirtualSize = 0;
    SectionTable[1].VirtualAddress = 0;
    SectionTable[1].SizeOfRawData = (LibraryNameWithExtension.size() + 1);
    SectionTable[1].PointerToRawData = sizeof(IMAGE_FILE_HEADER) + NumberOfSections * sizeof(IMAGE_SECTION_HEADER) +
                                       sizeof(IMAGE_IMPORT_DESCRIPTOR) + NumberOfRelocations * sizeof(IMAGE_RELOCATION);
    SectionTable[1].PointerToRelocations = 0;
    SectionTable[1].PointerToLinenumbers = 0;
    SectionTable[1].NumberOfRelocations = 0;
    SectionTable[1].PointerToLinenumbers = 0;
    SectionTable[1].Characteristics =
            IMAGE_SCN_ALIGN_2BYTES | IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;

    ResultObjectFileBuffer.Append(SectionTable, sizeof(SectionTable));

    // Populate data and relocations for .idata$2
    const IMAGE_IMPORT_DESCRIPTOR ImportDescriptor{};
    ResultObjectFileBuffer.Append(&ImportDescriptor, sizeof(ImportDescriptor));

    IMAGE_RELOCATION RelocationTable[NumberOfRelocations] = {};
    RelocationTable[0].VirtualAddress = offsetof(IMAGE_IMPORT_DESCRIPTOR, Name);
    RelocationTable[0].SymbolTableIndex = 2;
    RelocationTable[0].Type = GetImgRelRelocationForMachine(TargetMachine);

    RelocationTable[1].VirtualAddress = offsetof(IMAGE_IMPORT_DESCRIPTOR, OriginalFirstThunk);
    RelocationTable[1].SymbolTableIndex = 3;
    RelocationTable[1].Type = GetImgRelRelocationForMachine(TargetMachine);

    RelocationTable[2].VirtualAddress = offsetof(IMAGE_IMPORT_DESCRIPTOR, FirstThunk);
    RelocationTable[2].SymbolTableIndex = 4;
    RelocationTable[2].Type = GetImgRelRelocationForMachine(TargetMachine);

    ResultObjectFileBuffer.Append(RelocationTable, sizeof(RelocationTable));

    // Populate data for .idata$6
    ResultObjectFileBuffer.Append(LibraryNameWithExtension.c_str(), LibraryNameWithExtension.size() + 1);

    // Create COFF symbol table
    IMAGE_SYMBOL SymbolTable[NumberOfSymbols] = {};

    // Symbol for __IMPORT_DESCRIPTOR_ + LibraryName, which is an import descriptor for the library. It is located at offset 0 in section 1, which is .idata$2
    SymbolTable[0].Value = 0;
    SymbolTable[0].SectionNumber = 1;
    SymbolTable[0].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[0].StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
    SymbolTable[0].NumberOfAuxSymbols = 0;

    // Symbol for .idata$2 section contents. It contains dummy import descriptor for this import library
    memcpy(SymbolTable[1].N.ShortName, SectionNameIdata2, sizeof(SectionNameIdata2));
    SymbolTable[1].Value = 0;
    SymbolTable[1].SectionNumber = 1;
    SymbolTable[1].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[1].StorageClass = IMAGE_SYM_CLASS_SECTION;
    SymbolTable[1].NumberOfAuxSymbols = 0;

    // Symbol for .idata$6 section contents. It contains the name of the imported library
    memcpy(SymbolTable[2].N.ShortName, SectionNameIdata6, sizeof(SectionNameIdata6));
    SymbolTable[2].Value = 0;
    SymbolTable[2].SectionNumber = 2;
    SymbolTable[2].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[2].StorageClass = IMAGE_SYM_CLASS_STATIC;
    SymbolTable[2].NumberOfAuxSymbols = 0;

    // External symbol for .idata$2 section contents to pull it into the resulting image
    const BYTE SectionNameIdata4[]{'.', 'i', 'd', 'a', 't', 'a', '$', '4'};
    memcpy(SymbolTable[3].N.ShortName, SectionNameIdata4, sizeof(SectionNameIdata4));
    SymbolTable[3].Value = 0;
    SymbolTable[3].SectionNumber = IMAGE_SYM_UNDEFINED;
    SymbolTable[3].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[3].StorageClass = IMAGE_SYM_CLASS_SECTION;
    SymbolTable[3].NumberOfAuxSymbols = 0;

    // External symbol for .idata$5 section contents to pull it into the resulting image
    const BYTE SectionNameIdata5[]{'.', 'i', 'd', 'a', 't', 'a', '$', '5'};
    memcpy(SymbolTable[4].N.ShortName, SectionNameIdata5, sizeof(SectionNameIdata5));
    SymbolTable[4].Value = 0;
    SymbolTable[4].SectionNumber = IMAGE_SYM_UNDEFINED;
    SymbolTable[4].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[4].StorageClass = IMAGE_SYM_CLASS_SECTION;
    SymbolTable[4].NumberOfAuxSymbols = 0;

    // External symbol for __NULL_IMPORT_DESCRIPTOR to pull it into the resulting image
    SymbolTable[5].Value = 0;
    SymbolTable[5].SectionNumber = IMAGE_SYM_UNDEFINED;
    SymbolTable[5].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[5].StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
    SymbolTable[5].NumberOfAuxSymbols = 0;

    // External symbol for \x7f + LibraryName + _NULL_THUNK_DATA to pull it into the resulting image
    SymbolTable[6].Value = 0;
    SymbolTable[6].SectionNumber = IMAGE_SYM_UNDEFINED;
    SymbolTable[6].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[6].StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
    SymbolTable[6].NumberOfAuxSymbols = 0;

    // Fixup symbol names in the symbol table
    SymbolTable[0].N.Name.Long = sizeof(uint32_t);
    SymbolTable[5].N.Name.Long = sizeof(uint32_t) + ImportDescriptorSymbolName.size() + 1;
    SymbolTable[6].N.Name.Long = sizeof(uint32_t) + ImportDescriptorSymbolName.size() + 1 + NullImportDescriptorSymbolName.size() + 1;

    std::string NullThunkSymbolName(NullThunkDataPrefix);
    NullThunkSymbolName.append(LibraryName);
    NullThunkSymbolName.append(NullThunkDataSuffix);

    ResultObjectFileBuffer.Append(SymbolTable, sizeof(SymbolTable));

    // Write string table
    const uint32_t StringTableTotalSize = sizeof(uint32_t) + ImportDescriptorSymbolName.size() + NullImportDescriptorSymbolName.size() + NullThunkSymbolName.size() + 3;
    ResultObjectFileBuffer.Append(StringTableTotalSize);
    ResultObjectFileBuffer.Append(ImportDescriptorSymbolName.c_str(), ImportDescriptorSymbolName.size() + 1);
    ResultObjectFileBuffer.Append(NullImportDescriptorSymbolName.c_str(), NullImportDescriptorSymbolName.size() + 1);
    ResultObjectFileBuffer.Append(NullThunkSymbolName.c_str(), NullThunkSymbolName.size() + 1);

    return std::pair( LibraryNameWithExtension, std::move( ResultObjectFileBuffer ) );
}

std::pair<std::string, MemoryGrowableBuffer> COFFImportLibrary::CreateNullImportDescriptor() const
{
    const uint32_t NumberOfSections = 1;
    const uint32_t NumberOfSymbols = 1;

    MemoryGrowableBuffer ResultObjectFileBuffer;

    IMAGE_FILE_HEADER COFFFileHeader{};
    COFFFileHeader.Machine = TargetMachine;
    COFFFileHeader.NumberOfSections = NumberOfSections;
    COFFFileHeader.TimeDateStamp = 0;
    COFFFileHeader.PointerToSymbolTable = sizeof(COFFFileHeader) + (NumberOfSections * sizeof(IMAGE_SECTION_HEADER)) +
                                          // .idata$3
                                          sizeof(IMAGE_IMPORT_DESCRIPTOR);
    COFFFileHeader.NumberOfSymbols = NumberOfSymbols;
    COFFFileHeader.SizeOfOptionalHeader = 0;
    COFFFileHeader.Characteristics = 0;

    ResultObjectFileBuffer.Append(&COFFFileHeader, sizeof(COFFFileHeader));

    // Initialize section table
    IMAGE_SECTION_HEADER SectionTable[NumberOfSections] = {};

    // .idata$3
    const BYTE SectionNameIdata3[]{'.', 'i', 'd', 'a', 't', 'a', '$', '3'};
    memcpy(SectionTable[0].Name, SectionNameIdata3, sizeof(SectionNameIdata3));
    SectionTable[0].Misc.VirtualSize = 0;
    SectionTable[0].VirtualAddress = 0;
    SectionTable[0].SizeOfRawData = sizeof(IMAGE_IMPORT_DESCRIPTOR);
    SectionTable[0].PointerToRawData = sizeof(IMAGE_FILE_HEADER) + NumberOfSections * sizeof(IMAGE_SECTION_HEADER);
    SectionTable[0].PointerToRelocations = 0;
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].NumberOfRelocations = 0;
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].Characteristics =
            IMAGE_SCN_ALIGN_4BYTES | IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;

    ResultObjectFileBuffer.Append(SectionTable, sizeof(SectionTable));

    // Populate empty import descriptor for .idata$3
    const IMAGE_IMPORT_DESCRIPTOR ImportDescriptor{};
    ResultObjectFileBuffer.Append(&ImportDescriptor, sizeof(ImportDescriptor));

    // Create COFF symbol table
    IMAGE_SYMBOL SymbolTable[NumberOfSymbols] = {};

    // Symbol for __NULL_IMPORT_DESCRIPTOR to serve as a terminator for library imports
    SymbolTable[0].Value = 0;
    SymbolTable[0].SectionNumber = 1;
    SymbolTable[0].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[0].StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
    SymbolTable[0].NumberOfAuxSymbols = 0;

    // Write null descriptor symbol name offset
    SymbolTable[0].N.Name.Long = sizeof(uint32_t);
    ResultObjectFileBuffer.Append(SymbolTable, sizeof(SymbolTable));

    // Write string table
    const uint32_t StringTableTotalSize = sizeof(uint32_t) + NullImportDescriptorSymbolName.size() + 1;
    ResultObjectFileBuffer.Append(StringTableTotalSize);
    ResultObjectFileBuffer.Append(NullImportDescriptorSymbolName.c_str(), NullImportDescriptorSymbolName.size() + 1);

    return std::pair( LibraryNameWithExtension, std::move( ResultObjectFileBuffer ) );
}

std::pair<std::string, MemoryGrowableBuffer> COFFImportLibrary::CreateNullThunk() const
{
    const uint32_t NumberOfSections = 2;
    const uint32_t NumberOfSymbols = 1;
    const uint32_t VASize = sizeof(uint64_t);

    MemoryGrowableBuffer ResultObjectFileBuffer;

    IMAGE_FILE_HEADER COFFFileHeader{};
    COFFFileHeader.Machine = TargetMachine;
    COFFFileHeader.NumberOfSections = NumberOfSections;
    COFFFileHeader.TimeDateStamp = 0;
    COFFFileHeader.PointerToSymbolTable = sizeof(COFFFileHeader) + (NumberOfSections * sizeof(IMAGE_SECTION_HEADER)) +
        // .idata$5
        VASize +
        // .idata$4
        VASize;
    COFFFileHeader.NumberOfSymbols = NumberOfSymbols;
    COFFFileHeader.SizeOfOptionalHeader = 0;
    COFFFileHeader.Characteristics = 0;

    ResultObjectFileBuffer.Append(&COFFFileHeader, sizeof(COFFFileHeader));

    // Initialize section table
    IMAGE_SECTION_HEADER SectionTable[NumberOfSections] = {};

    // .idata$5
    const BYTE SectionNameIdata5[] {'.', 'i', 'd', 'a', 't', 'a', '$', '5'};
    memcpy(SectionTable[0].Name, SectionNameIdata5, sizeof(SectionNameIdata5));
    SectionTable[0].Misc.VirtualSize = 0;
    SectionTable[0].VirtualAddress = 0;
    SectionTable[0].SizeOfRawData = VASize;
    SectionTable[0].PointerToRawData = sizeof(IMAGE_FILE_HEADER) + NumberOfSections * sizeof(IMAGE_SECTION_HEADER);
    SectionTable[0].PointerToRelocations = 0;
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].NumberOfRelocations = 0;
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].Characteristics = IMAGE_SCN_ALIGN_8BYTES | IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;

    // .idata$4
    const BYTE SectionNameIdata4[] {'.', 'i', 'd', 'a', 't', 'a', '$', '5'};
    memcpy(SectionTable[0].Name, SectionNameIdata4, sizeof(SectionNameIdata4));
    SectionTable[0].Misc.VirtualSize = 0;
    SectionTable[0].VirtualAddress = 0;
    SectionTable[0].SizeOfRawData = VASize;
    SectionTable[0].PointerToRawData = sizeof(IMAGE_FILE_HEADER) + NumberOfSections * sizeof(IMAGE_SECTION_HEADER) + VASize;
    SectionTable[0].PointerToRelocations = 0;
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].NumberOfRelocations = 0;
    SectionTable[0].PointerToLinenumbers = 0;
    SectionTable[0].Characteristics = IMAGE_SCN_ALIGN_8BYTES | IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;

    ResultObjectFileBuffer.Append(SectionTable, sizeof(SectionTable));

    // Append data for sections
    const uint64_t ILTTerminator = 0;
    ResultObjectFileBuffer.Append(ILTTerminator);
    const uint64_t IATTerminator = 0;
    ResultObjectFileBuffer.Append(IATTerminator);

    // Create COFF symbol table
    IMAGE_SYMBOL SymbolTable[NumberOfSymbols] = {};

    // Symbol for LibraryName + _NULL_THUNK_DATA to serve as a terminator for library imports
    SymbolTable[0].Value = 0;
    SymbolTable[0].SectionNumber = 1;
    SymbolTable[0].Type = IMAGE_SYM_TYPE_NULL;
    SymbolTable[0].StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
    SymbolTable[0].NumberOfAuxSymbols = 0;

    // Write null descriptor symbol name offset
    SymbolTable[0].N.Name.Long = sizeof(uint32_t);
    ResultObjectFileBuffer.Append(SymbolTable, sizeof(SymbolTable));

    // Write string table
    const uint32_t StringTableTotalSize = sizeof(uint32_t) + NullThunkSymbolName.size() + 1;
    ResultObjectFileBuffer.Append(StringTableTotalSize);
    ResultObjectFileBuffer.Append(NullThunkSymbolName.c_str(), NullThunkSymbolName.size() + 1);

    return std::pair( LibraryNameWithExtension, std::move( ResultObjectFileBuffer ) );
}

void COFFImportLibrary::DiscoverSymbols(std::vector<std::pair<std::string, uint64_t>>& OutSymbolToMemberIndex) const
{
    for ( int32_t i = 0; i < Members.size(); i++ )
    {
        const MemoryGrowableBuffer& MemberBuffer = Members[i].second;

        const IMAGE_FILE_HEADER* ImageFileHeader = reinterpret_cast<const IMAGE_FILE_HEADER*>( MemberBuffer.GetRawBuffer() );

        // Check if this object file is short import and we should not attempt to parse it as full object file
        if ( ImageFileHeader->Machine == IMAGE_FILE_MACHINE_UNKNOWN && ImageFileHeader->NumberOfSections == IMPORT_OBJECT_HDR_SIG2 )
        {
            const IMPORT_OBJECT_HEADER* ImportObjectHeader = reinterpret_cast<const IMPORT_OBJECT_HEADER*>( MemberBuffer.GetRawBuffer() );
            const char* StringDataStart = reinterpret_cast<const char*>( MemberBuffer.GetRawBuffer() + sizeof(IMPORT_OBJECT_HEADER) );
            const char* ImportNameStringDataEnd = static_cast<const char*>( memchr( StringDataStart, '\0', ImportObjectHeader->SizeOfData ) );

            assert( ImportNameStringDataEnd != nullptr && L"Failed to parse short import name from the member" );
            if ( ImportNameStringDataEnd != nullptr )
            {
                const std::string ImportSymbolName( StringDataStart, ImportNameStringDataEnd - StringDataStart );
                OutSymbolToMemberIndex.push_back( std::pair( ImportSymbolName, i ) );
            }
            continue;
        }

        const IMAGE_SYMBOL* SymbolTable = reinterpret_cast<const IMAGE_SYMBOL*>( MemberBuffer.GetRawBuffer() + ImageFileHeader->PointerToSymbolTable );
        const uint8_t* StringTableStart = reinterpret_cast<const uint8_t*>( SymbolTable + ImageFileHeader->NumberOfSymbols );

        for ( int32_t SymbolIndex = 0; SymbolIndex < ImageFileHeader->NumberOfSymbols; SymbolIndex++ )
        {
            const IMAGE_SYMBOL& ImageSymbol = SymbolTable[SymbolIndex];

            // If short name is not zero, use the short name
            if ( ImageSymbol.N.Name.Short != 0 )
            {
                const char* NameStringStart = reinterpret_cast<const char*>( ImageSymbol.N.ShortName );
                const char* NameStringEnd = static_cast<const char*>( memchr( NameStringStart, '\0', sizeof(ImageSymbol.N.ShortName) ) );

                // If we failed to find a null terminator in the short name buffer, the symbol name is full 8 characters
                if ( NameStringEnd == nullptr )
                {
                    NameStringEnd = NameStringStart + sizeof(ImageSymbol.N.ShortName);
                }
                const std::string ShortSymbolName( NameStringStart, NameStringEnd - NameStringStart );
                OutSymbolToMemberIndex.push_back( std::pair( ShortSymbolName, i ) );
                continue;
            }

            // If short name is zero, we need to use the offset into the string table
            const uint32_t StringTableSize = *reinterpret_cast<const uint32_t*>( &StringTableStart[0] );
            const uint32_t StringTableNameOffset = ImageSymbol.N.Name.Long;
            assert( StringTableNameOffset < StringTableSize && L"Invalid long symbol name offset outside of the string table bounds" );

            const char* NameStringStart = reinterpret_cast<const char*>( StringTableStart + StringTableNameOffset );
            const char* NameStringEnd = static_cast<const char*>( memchr( NameStringStart, '\0', StringTableSize - StringTableNameOffset ) );

            assert( NameStringEnd != nullptr && L"Invalid non-null-terminated symbol name in the string table" );
            if ( NameStringEnd != nullptr && StringTableNameOffset < StringTableSize )
            {
                const std::string LongSymbolName( NameStringStart, NameStringEnd - NameStringStart );
                OutSymbolToMemberIndex.push_back( std::pair( LongSymbolName, i ) );
            }
        }
    }
}

bool COFFImportLibrary::WriteLibrary(const std::wstring& InFilename)
{
    std::vector<std::pair<std::string, uint64_t>> AllSymbols;
    DiscoverSymbols( AllSymbols );

    MemoryGrowableBuffer LongNamesMemberBuffer;
    std::unordered_map<std::string, uint64_t> LongArchiveNameToOffset;

    MemoryGrowableBuffer CombinedArchiveMembersBuffer;
    std::unordered_map<uint64_t, uint64_t> MemberIndexToMemberOffsetFromArchiveMembersStart;

    // Append all members into one large buffer and calculate offsets to them. Also keep track of long names
    const uint64_t NumberOfMembers = Members.size();
    for ( int32_t MemberIndex = 0; MemberIndex < NumberOfMembers; MemberIndex++ )
    {
        const auto& [MemberName, MemberBuffer] = Members[MemberIndex];

        MemberIndexToMemberOffsetFromArchiveMembersStart.insert({ MemberIndex, CombinedArchiveMembersBuffer.Tell() });
        const std::string MemberSize = std::to_string(MemberBuffer.Tell());

        // Initialize member header with all spaces since it is a fully textual header
        IMAGE_ARCHIVE_MEMBER_HEADER MemberHeader;
        memset( &MemberHeader, ' ', sizeof(MemberHeader) );
        memcpy( MemberHeader.Date, "-1", 2 );
        memcpy( MemberHeader.Mode, "0", 1 );
        assert( MemberSize.size() <= sizeof(MemberHeader.Size) );
        memcpy( MemberHeader.Size, MemberSize.data(), MemberSize.size() );
        memcpy( MemberHeader.EndHeader, IMAGE_ARCHIVE_END, sizeof(MemberHeader.EndHeader) );

        // If name plus the terminator character does not fit into the inline name buffer, we need to write it into the long names section instead
        if ( MemberName.size() >= sizeof(MemberHeader.Name) )
        {
            // Make sure the long name is written into the long names buffer
            if ( !LongArchiveNameToOffset.contains( MemberName ) )
            {
                const uint64_t CurrentLongNamesOffset = LongNamesMemberBuffer.Tell();
                LongArchiveNameToOffset.insert({ MemberName, CurrentLongNamesOffset });
                LongNamesMemberBuffer.Append( MemberName.c_str(), MemberName.size() + 1 );
            }

            // Write offset into the long names section
            const uint64_t LongNameOffset = LongArchiveNameToOffset.find( MemberName )->second;
            const std::string LongNameOffsetString = std::to_string( LongNameOffset );

            MemberHeader.Name[0] = '/';
            assert( LongNameOffsetString.size() < sizeof(MemberHeader.Name) );
            memcpy( MemberHeader.Name + 1, LongNameOffsetString.data(), LongNameOffsetString.size() );
        }
        // Otherwise, the name fits inline into the archive member name field
        else
        {
            assert( MemberName.size() < sizeof(MemberHeader.Name) );
            memcpy( MemberHeader.Name, MemberName.data(), MemberName.size() );
            MemberHeader.Name[MemberName.size()] = '/';
        }

        CombinedArchiveMembersBuffer.Append( &MemberHeader, sizeof(MemberHeader) );
        CombinedArchiveMembersBuffer.Append( MemberBuffer );

        // Pad the trailer of this member optionally to always be 2-byte aligned
        if ( CombinedArchiveMembersBuffer.Tell() % 2 == 1 )
        {
            CombinedArchiveMembersBuffer.Append( IMAGE_ARCHIVE_PAD, 1 );
        }
    }
    // Make sure to pad long names member size to be on even byte boundary
    if ( LongNamesMemberBuffer.Tell() % 2 == 1 )
    {
        LongNamesMemberBuffer.Append( IMAGE_ARCHIVE_PAD, 1 );
    }

    // Calculate total length of the symbol string table. This is necessary to be able to compute offsets to member headers
    const uint64_t NumberOfSymbols = AllSymbols.size();
    uint64_t TotalSymbolsStringTableSize = 0;
    for (const std::string& SymbolName : AllSymbols | std::views::keys )
    {
        TotalSymbolsStringTableSize += SymbolName.size() + 1;
    }
    // Pad string table size, since it might not be on an even byte boundary
    TotalSymbolsStringTableSize += TotalSymbolsStringTableSize % 2;

    // Build resulting import library
    MemoryGrowableBuffer ImportLibraryBuffer;
    ImportLibraryBuffer.AppendChar(IMAGE_ARCHIVE_START, IMAGE_ARCHIVE_START_SIZE);

    const uint64_t EstimatedFirstLinkerMemberSize = sizeof(IMAGE_ARCHIVE_MEMBER_HEADER) + sizeof(uint32_t) + NumberOfSymbols * sizeof(uint32_t) + TotalSymbolsStringTableSize;
    const uint64_t EstimatedSecondLinkerMemberSize = sizeof(IMAGE_ARCHIVE_MEMBER_HEADER) + sizeof(uint32_t) + NumberOfMembers * sizeof(uint32_t) +
        sizeof(uint32_t) + NumberOfSymbols * sizeof(uint16_t) + TotalSymbolsStringTableSize;
    const uint64_t EstimatedLongNamesMemberSize = sizeof(IMAGE_ARCHIVE_MEMBER_HEADER) + LongNamesMemberBuffer.Tell();

    const uint64_t OffsetToStartOfArchiveMembersBlock = IMAGE_ARCHIVE_START_SIZE + EstimatedFirstLinkerMemberSize + EstimatedSecondLinkerMemberSize + EstimatedLongNamesMemberSize;

    {
        // Build first linker member
        MemoryGrowableBuffer FirstLinkerMemberBuffer;

        // Write number of symbols
        // This is Big Endian!!! So need to flip the bytes
        assert( NumberOfSymbols <= UINT32_MAX && L"Only up to 2 billion symbols are supported in archive library" );
        const uint32_t NumberOfSymbolsUint32BE = _byteswap_ulong( static_cast<uint32_t>( NumberOfSymbols ) );
        FirstLinkerMemberBuffer.Append( NumberOfSymbolsUint32BE );

        // Write offset to the start of archive member for each symbol. We have to compensate for the offsets being relative to the start of archive members block
        for ( int32_t i = 0; i < NumberOfSymbols; i++ )
        {
            const uint64_t OffsetFromArchiveMembersStart = MemberIndexToMemberOffsetFromArchiveMembersStart.find( AllSymbols[i].second )->second;
            const uint64_t TotalOffsetToStartOfArchiveMember = OffsetToStartOfArchiveMembersBlock + OffsetFromArchiveMembersStart;
            assert( TotalOffsetToStartOfArchiveMember <= UINT32_MAX && L"Large objects are not supported" );

            // This is Big Endian!!! So need to flip the bytes
            const uint32_t TotalOffsetToStartOfArchiveMemberUint32BE = _byteswap_ulong( static_cast<uint32_t>( TotalOffsetToStartOfArchiveMember ) );
            FirstLinkerMemberBuffer.Append( TotalOffsetToStartOfArchiveMemberUint32BE );
        }

        // Write symbol names string table
        for ( int32_t i = 0; i < NumberOfSymbols; i++ )
        {
            const std::string& SymbolName = AllSymbols[i].first;
            FirstLinkerMemberBuffer.Append( SymbolName.c_str(), SymbolName.size() + 1 );
        }
        // Make sure the trailer of first linker member is, like for all other members, 2-byte aligned
        if ( FirstLinkerMemberBuffer.Tell() % 2 == 1 )
        {
            FirstLinkerMemberBuffer.Append( IMAGE_ARCHIVE_PAD, 1 );
        }

        // Build archive member header for first linker member
        IMAGE_ARCHIVE_MEMBER_HEADER FirstLinkerMemberHeader;
        memset( &FirstLinkerMemberHeader, ' ', sizeof(FirstLinkerMemberHeader) );
        memcpy( FirstLinkerMemberHeader.Name, IMAGE_ARCHIVE_LINKER_MEMBER, sizeof(FirstLinkerMemberHeader.Name) );
        memcpy( FirstLinkerMemberHeader.Date, "-1", 2 );
        memcpy( FirstLinkerMemberHeader.Mode, "0", 1 );
        memcpy( FirstLinkerMemberHeader.EndHeader, IMAGE_ARCHIVE_END, sizeof(FirstLinkerMemberHeader.EndHeader) );

        // Calculate and write the size of the first linker member
        assert( FirstLinkerMemberBuffer.Tell() + sizeof(FirstLinkerMemberHeader) == EstimatedFirstLinkerMemberSize );

        const std::string FirstLinkerMemberSizeString = std::to_string(FirstLinkerMemberBuffer.Tell());
        assert( FirstLinkerMemberSizeString.size() <= sizeof(FirstLinkerMemberHeader.Size) );
        memcpy( FirstLinkerMemberHeader.Size, FirstLinkerMemberSizeString.data(), FirstLinkerMemberSizeString.size() );

        // Append member header and the payload
        ImportLibraryBuffer.Append( &FirstLinkerMemberHeader, sizeof(FirstLinkerMemberHeader) );
        ImportLibraryBuffer.Append( FirstLinkerMemberBuffer );
    }

    {
        // Build second linker member
        MemoryGrowableBuffer SecondLinkerMemberBuffer;

        // Write number of members
        assert( NumberOfMembers <= UINT32_MAX && L"Only up to 2 billion members are supported in archive library" );
        const uint32_t NumberOfMembersUint32 = static_cast<uint32_t>( NumberOfMembers );
        SecondLinkerMemberBuffer.Append( NumberOfMembersUint32 );

        // Write offsets to the start of each archive member
        for ( int32_t MemberIndex = 0; MemberIndex < NumberOfMembers; MemberIndex++ )
        {
            const uint64_t OffsetFromArchiveMembersStart = MemberIndexToMemberOffsetFromArchiveMembersStart.find( MemberIndex )->second;
            const uint64_t TotalOffsetToStartOfArchiveMember = OffsetToStartOfArchiveMembersBlock + OffsetFromArchiveMembersStart;
            assert( TotalOffsetToStartOfArchiveMember <= UINT32_MAX && L"Large objects are not supported" );

            const uint32_t TotalOffsetToStartOfArchiveMemberUint32 = static_cast<uint32_t>( TotalOffsetToStartOfArchiveMember );
            SecondLinkerMemberBuffer.Append( TotalOffsetToStartOfArchiveMemberUint32 );
        }

        // Write number of symbols
        assert( NumberOfSymbols <= UINT32_MAX && L"Only up to 2 billion symbols are supported in archive library" );
        const uint32_t NumberOfSymbolsUint32 = static_cast<uint32_t>( NumberOfSymbols );
        SecondLinkerMemberBuffer.Append( NumberOfSymbolsUint32 );

        // Sort symbols lexicographically. This is a requirement for the second linker member
        std::ranges::stable_sort(AllSymbols, []( const std::pair<std::string, uint64_t>& A, const std::pair<std::string, uint64_t>& B )
        {
            return A.first < B.first;
        } );

        // Write indices from symbol table into the member offsets array
        for ( int32_t i = 0; i < NumberOfSymbols; i++ )
        {
            const uint64_t OneBasedMemberIndex = 1 + AllSymbols[i].second;
            assert( OneBasedMemberIndex <= UINT16_MAX && L"Only up to 32k members are supported in import archive libraries" );

            const uint16_t MemberIndexUint16 = static_cast<uint16_t>( OneBasedMemberIndex );
            SecondLinkerMemberBuffer.Append( MemberIndexUint16 );
        }

        // Write symbol names string table
        for ( int32_t i = 0; i < NumberOfSymbols; i++ )
        {
            const std::string& SymbolName = AllSymbols[i].first;
            SecondLinkerMemberBuffer.Append( SymbolName.c_str(), SymbolName.size() + 1 );
        }
        // Make sure the trailer of second linker member is, like for all other members, 2-byte aligned
        if ( SecondLinkerMemberBuffer.Tell() % 2 == 1 )
        {
            SecondLinkerMemberBuffer.Append( IMAGE_ARCHIVE_PAD, 1 );
        }

        // Build archive member header for first linker member
        IMAGE_ARCHIVE_MEMBER_HEADER SecondLinkerMemberHeader;
        memset( &SecondLinkerMemberHeader, ' ', sizeof(SecondLinkerMemberHeader) );
        memcpy( SecondLinkerMemberHeader.Name, IMAGE_ARCHIVE_LINKER_MEMBER, sizeof(SecondLinkerMemberHeader.Name) );
        memcpy( SecondLinkerMemberHeader.Date, "-1", 2 );
        memcpy( SecondLinkerMemberHeader.Mode, "0", 1 );
        memcpy( SecondLinkerMemberHeader.EndHeader, IMAGE_ARCHIVE_END, sizeof(SecondLinkerMemberHeader.EndHeader) );

        // Calculate and write the size of the first linker member
        assert( SecondLinkerMemberBuffer.Tell() + sizeof(SecondLinkerMemberHeader) == EstimatedSecondLinkerMemberSize );

        const std::string SecondLinkerMemberSizeString = std::to_string(SecondLinkerMemberBuffer.Tell());
        assert( SecondLinkerMemberSizeString.size() <= sizeof(SecondLinkerMemberHeader.Size) );
        memcpy( SecondLinkerMemberHeader.Size, SecondLinkerMemberSizeString.data(), SecondLinkerMemberSizeString.size() );

        // Append member header and the payload
        ImportLibraryBuffer.Append( &SecondLinkerMemberHeader, sizeof(SecondLinkerMemberHeader) );
        ImportLibraryBuffer.Append( SecondLinkerMemberBuffer );
    }

    {
        // Build long names member
        IMAGE_ARCHIVE_MEMBER_HEADER LongNamesMemberHeader;
        memset( &LongNamesMemberHeader, ' ', sizeof(LongNamesMemberHeader) );
        memcpy( LongNamesMemberHeader.Name, IMAGE_ARCHIVE_LONGNAMES_MEMBER, sizeof(LongNamesMemberHeader.Name) );
        memcpy( LongNamesMemberHeader.Date, "-1", 2 );
        memcpy( LongNamesMemberHeader.Mode, "0", 1 );
        memcpy( LongNamesMemberHeader.EndHeader, IMAGE_ARCHIVE_END, sizeof(LongNamesMemberHeader.EndHeader) );

        assert( LongNamesMemberBuffer.Tell() + sizeof(LongNamesMemberHeader) == EstimatedLongNamesMemberSize );

        const std::string LongNamesMemberSizeString = std::to_string(LongNamesMemberBuffer.Tell());
        assert( LongNamesMemberSizeString.size() <= sizeof(LongNamesMemberHeader.Size) );
        memcpy( LongNamesMemberHeader.Size, LongNamesMemberSizeString.data(), LongNamesMemberSizeString.size() );

        // Append member header and the payload
        ImportLibraryBuffer.Append( &LongNamesMemberHeader, sizeof(LongNamesMemberHeader) );
        ImportLibraryBuffer.Append( LongNamesMemberBuffer );
    }

    assert( OffsetToStartOfArchiveMembersBlock == ImportLibraryBuffer.Tell() && L"Invalid library headers size calculated" );
    // Append the rest of the member chunks
    ImportLibraryBuffer.Append( CombinedArchiveMembersBuffer );

    // Write the resulting library to the file
    return ImportLibraryBuffer.WriteToFile( std::filesystem::path( InFilename ) );
}
