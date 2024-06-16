#include "Utils/MemoryGrowableBuffer.h"

#include <assert.h>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>

MemoryGrowableBuffer::MemoryGrowableBuffer()
{
}

MemoryGrowableBuffer::MemoryGrowableBuffer(MemoryGrowableBuffer&& Other) noexcept : BufferData( Other.BufferData ), CurrentOffset( Other.CurrentOffset ), CurrentBufferSize( Other.CurrentBufferSize )
{
    Other.BufferData = nullptr;
    Other.CurrentOffset = 0;
    Other.CurrentBufferSize = 0;
}

MemoryGrowableBuffer::~MemoryGrowableBuffer()
{
    if ( BufferData )
    {
        free( BufferData );
        CurrentBufferSize = 0;
    }
}

MemoryGrowableBuffer& MemoryGrowableBuffer::operator=(MemoryGrowableBuffer&& Other) noexcept
{
    if ( &Other != this )
    {
        std::swap( BufferData, Other.BufferData );
        std::swap( CurrentOffset, Other.CurrentOffset );
        std::swap( CurrentBufferSize, Other.CurrentBufferSize );
    }
    return *this;
}

void MemoryGrowableBuffer::Seek(const uint64_t NewOffset)
{
    assert( NewOffset <= CurrentBufferSize );
    CurrentOffset = NewOffset;
}

void MemoryGrowableBuffer::Skip(const uint64_t NumToSkip)
{
    assert( CurrentOffset + NumToSkip <= CurrentBufferSize );
    CurrentOffset += NumToSkip;
}

void MemoryGrowableBuffer::Grow(const uint64_t GrowAmount)
{
    CurrentBufferSize += (GrowAmount + BufferGrowSizeMultiple - 1) / BufferGrowSizeMultiple * BufferGrowSizeMultiple;
    BufferData = static_cast<uint8_t*>(realloc(BufferData, CurrentBufferSize));
}

void MemoryGrowableBuffer::EnsureCapacity(const uint64_t CapacityNeeded)
{
    if ( CurrentOffset + CapacityNeeded >= CurrentBufferSize )
    {
        Grow( CapacityNeeded );
    }
}

void MemoryGrowableBuffer::Append(const void* InData, const uint64_t InDataSize)
{
    EnsureCapacity(InDataSize);
    memcpy( BufferData + CurrentOffset, InData, InDataSize );
    CurrentOffset += InDataSize;
}

void MemoryGrowableBuffer::Append(const MemoryGrowableBuffer& OtherBuffer)
{
    EnsureCapacity(OtherBuffer.Tell());
    memcpy(BufferData + CurrentOffset, OtherBuffer.GetRawBuffer(), OtherBuffer.Tell());
    CurrentOffset += OtherBuffer.Tell();
}

void MemoryGrowableBuffer::Copy(const void* InData, const uint64_t InDataSize)
{
    EnsureCapacity(InDataSize);
    memcpy( BufferData + CurrentOffset, InData, InDataSize );
}

bool MemoryGrowableBuffer::WriteToFile(const std::filesystem::path& FilePath) const
{
    std::ofstream FileStream(FilePath, std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    if (!FileStream.good())
    {
        std::wcerr << L"Failed to open file for writing: " << FilePath.wstring() << std::endl;
        return false;
    }
    FileStream.write( reinterpret_cast<const char*>(BufferData), CurrentOffset );
    FileStream.flush();
    return true;
}
