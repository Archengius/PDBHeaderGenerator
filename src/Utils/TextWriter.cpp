#define _CRT_SECURE_NO_WARNINGS
#include "Utils/TextWriter.h"

#include <cassert>
#include <cstdarg>
#include <fstream>
#include <iostream>

FormattedTextWriter& FormattedTextWriter::Append(const std::wstring& InString)
{
    PlaceNewlineIndent();
    WriterBuffer << InString;
    return *this;
}

FormattedTextWriter& FormattedTextWriter::Append(const wchar_t* InLiteral)
{
    PlaceNewlineIndent();
    WriterBuffer << InLiteral;
    return *this;
}

FormattedTextWriter& FormattedTextWriter::AppendNewline(const std::wstring& InString)
{
    PlaceNewlineIndent();
    WriterBuffer << InString;
    AppendNewline();
    return *this;
}

FormattedTextWriter& FormattedTextWriter::AppendNewline(const wchar_t* InLiteral)
{
    PlaceNewlineIndent();
    WriterBuffer << InLiteral;
    AppendNewline();
    return *this;
}

FormattedTextWriter& FormattedTextWriter::AppendFormat( const wchar_t* InFormat, ... )
{
    va_list ArgumentList;
    va_start( ArgumentList, InFormat );
    const int32_t BufferSize = _vsnwprintf( nullptr, 0, InFormat, ArgumentList ) + 1;

    wchar_t* FormatBuffer = static_cast<wchar_t*>(malloc(BufferSize * sizeof(wchar_t)));
    memset( FormatBuffer, 0, BufferSize * sizeof(wchar_t) );
    _vsnwprintf( FormatBuffer, BufferSize, InFormat, ArgumentList );

    PlaceNewlineIndent();
    WriterBuffer << FormatBuffer;

    free( FormatBuffer );
    va_end( ArgumentList );
    return *this;
}

FormattedTextWriter& FormattedTextWriter::AppendFormatNewline( const wchar_t* InFormat, ... )
{
    va_list ArgumentList;
    va_start( ArgumentList, InFormat );
    const int32_t BufferSize = _vsnwprintf( nullptr, 0, InFormat, ArgumentList ) + 1;

    wchar_t* FormatBuffer = static_cast<wchar_t*>(malloc(BufferSize * sizeof(wchar_t)));
    memset( FormatBuffer, 0, BufferSize * sizeof(wchar_t) );
    _vsnwprintf( FormatBuffer, BufferSize, InFormat, ArgumentList );

    PlaceNewlineIndent();
    WriterBuffer << FormatBuffer;
    AppendNewline();

    free( FormatBuffer );
    va_end( ArgumentList );
    return *this;
}

FormattedTextWriter& FormattedTextWriter::AppendFormatNewlineNoIndent(const wchar_t* InFormat, ...)
{
    va_list ArgumentList;
    va_start( ArgumentList, InFormat );
    const int32_t BufferSize = _vsnwprintf( nullptr, 0, InFormat, ArgumentList ) + 1;

    wchar_t* FormatBuffer = static_cast<wchar_t*>(malloc(BufferSize * sizeof(wchar_t)));
    memset( FormatBuffer, 0, BufferSize * sizeof(wchar_t) );
    _vsnwprintf( FormatBuffer, BufferSize, InFormat, ArgumentList );

    PlaceNewlineIndent(1);
    WriterBuffer << FormatBuffer;
    AppendNewline();

    free( FormatBuffer );
    va_end( ArgumentList );
    return *this;
}

FormattedTextWriter& FormattedTextWriter::AppendNewline()
{
    PlaceNewlineIndent();
    WriterBuffer << std::endl;
    ResetNewlineIndent();
    return *this;
}

void FormattedTextWriter::PlaceNewlineIndent( int32_t IndentLevelsToIgnore )
{
    if ( !bNewlineIndentPlaced )
    {
        if ( IndentLevelsToIgnore > CurrentIndentionLevel )
        {
            IndentLevelsToIgnore = CurrentIndentionLevel;
        }
        WriterBuffer << std::wstring( CurrentIndentionLevel - IndentLevelsToIgnore, L'\t' );
        bNewlineIndentPlaced = true;
    }
}

void FormattedTextWriter::ResetNewlineIndent()
{
    bNewlineIndentPlaced = false;
}

void FormattedTextWriter::IncrementIndentionLevel()
{
    CurrentIndentionLevel++;
}

void FormattedTextWriter::DecrementIndentionLevel()
{
    assert( CurrentIndentionLevel > 0 );
    CurrentIndentionLevel--;
}

void FormattedTextWriter::WriteToFile(const std::filesystem::path& FilePath) const
{
    std::wofstream FileStream( FilePath, std::ios_base::out | std::ios_base::trunc );

    if (!FileStream.good())
    {
        std::wcerr << L"Failed to open file for read: " << FilePath.wstring() << std::endl;
        return;
    }
    const std::wstring ResultText = WriterBuffer.str();
    FileStream.write( ResultText.c_str(), ResultText.size() );
    FileStream.flush();
    FileStream.close();
}

std::wstring FormattedTextWriter::ToString() const
{
    return WriterBuffer.str();
}
