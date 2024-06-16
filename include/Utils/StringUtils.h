#pragma once

#include <sstream>
#include <string>

std::wstring ConvertMbStringToWide( const char* InString );

std::wstring StringPrintf( const wchar_t* InFormat, ... );

std::wstring StringUtf8ToWide( const std::string& InUtf8String );

template<typename TRangeType, typename TCallable>
std::wstring JoinToString( const TRangeType& RangeType, const std::wstring& InDelimeter, const TCallable& Callable)
{
    std::wostringstream ResultString;
    bool bFirstElement = true;
    for ( const auto& Element : RangeType )
    {
        if ( !bFirstElement )
        {
            ResultString << InDelimeter;
        }
        bFirstElement = false;
        ResultString << Callable( Element );
    }
    return ResultString.str();
}

std::wstring ReplaceAll(std::wstring str, const std::wstring& from, const std::wstring& to);

template<typename TRangeType>
std::wstring JoinToString( const TRangeType& RangeType, const std::wstring& InDelimeter)
{
    std::wostringstream ResultString;
    bool bFirstElement = true;
    for ( const auto& Element : RangeType )
    {
        if ( !bFirstElement )
        {
            ResultString << InDelimeter;
        }
        bFirstElement = false;
        ResultString << Element;
    }
    return ResultString.str();
}

// TODO: This is not quite string utils, this should be in a separate header.

template <class T>
void HashCombine(std::size_t& seed, const T& v)
{
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

template<typename T1, typename T2>
struct std::hash<std::pair<T1, T2>>
{
    size_t operator()(const std::pair<T1, T2>& Pair) const noexcept
    {
        std::size_t ResultHash = std::hash<T1>()(Pair.first);
        HashCombine(ResultHash, Pair.second);
        return ResultHash;
    }
};
