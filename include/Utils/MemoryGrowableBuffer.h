#pragma once

#include <cstdint>
#include <filesystem>
#include <type_traits>

class MemoryGrowableBuffer
{
    uint8_t* BufferData{};
    uint64_t CurrentOffset{};
    uint64_t CurrentBufferSize{};
    static constexpr uint64_t BufferGrowSizeMultiple = 4096;
public:
    MemoryGrowableBuffer();
    // We do not want to have growable buffers copied implicitly.
    MemoryGrowableBuffer( const MemoryGrowableBuffer& Other ) = delete;
    MemoryGrowableBuffer( MemoryGrowableBuffer&& Other ) noexcept;
    ~MemoryGrowableBuffer();

    MemoryGrowableBuffer& operator=( const MemoryGrowableBuffer& Other ) = delete;
    MemoryGrowableBuffer& operator=( MemoryGrowableBuffer&& Other ) noexcept;

    uint64_t Tell() const { return CurrentOffset; }
    uint64_t Max() const { return CurrentBufferSize; }
    uint64_t Available() const { return CurrentBufferSize - CurrentOffset; }
    uint8_t* GetRawBuffer() const { return BufferData; }

    void Seek( uint64_t NewOffset );
    void Skip( uint64_t NumToSkip );
    void Grow( uint64_t GrowAmount );

    void EnsureCapacity( uint64_t CapacityNeeded );
    void Append( const void* InData, uint64_t InDataSize );
    void Copy( const void* InData, uint64_t InDataSize );

    void AppendChar( const char* InData, const uint64_t InDataSize )
    {
        Append( InData, InDataSize );
    }

    void Append( const MemoryGrowableBuffer& OtherBuffer );

    template<typename T>
    void Append( const T& InData )
    {
        static_assert( std::is_pod_v<T>, "Only POD types can be appended to the buffer memory directly" );
        Append( &InData, sizeof(T) );
    }

    bool WriteToFile(const std::filesystem::path& FilePath) const;
};
