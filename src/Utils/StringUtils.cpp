#define _CRT_SECURE_NO_WARNINGS
#define _SILENCE_CXX17_CODECVT_HEADER_DEPRECATION_WARNING
#include "Utils/StringUtils.h"
#include <cstdarg>
#include <codecvt>

std::wstring ConvertMbStringToWide( const char* InString )
{
    wchar_t ConvertedBuffer[1024];
    ConvertedBuffer[0] = '\0';
    mbstowcs_s( nullptr, ConvertedBuffer, InString, _TRUNCATE );
    return ConvertedBuffer;
}

std::wstring ReplaceAll(std::wstring str, const std::wstring& from, const std::wstring& to) {
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // Handles case where 'to' is a substring of 'from'
    }
    return str;
}

std::wstring StringPrintf( const wchar_t* InFormat, ... )
{
    va_list ArgumentList;
    va_start( ArgumentList, InFormat );
    const int32_t BufferSize = _vsnwprintf( nullptr, 0, InFormat, ArgumentList ) + 1;

    wchar_t* FormatBuffer = static_cast<wchar_t*>(malloc(BufferSize * sizeof(wchar_t)));
    memset( FormatBuffer, 0, BufferSize * sizeof(wchar_t) );
    _vsnwprintf( FormatBuffer, BufferSize, InFormat, ArgumentList );

    std::wstring ResultString = FormatBuffer;
    free( FormatBuffer );
    va_end( ArgumentList );
    return ResultString;
}

std::wstring StringUtf8ToWide( const std::string& InUtf8String )
{
    std::wstring_convert<std::codecvt_utf8<wchar_t>> Codecvt;
    return Codecvt.from_bytes( InUtf8String );
}
