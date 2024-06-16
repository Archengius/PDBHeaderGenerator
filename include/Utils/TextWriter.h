#pragma once

#include <cstdint>
#include <sstream>
#include <filesystem>

class FormattedTextWriter
{
    std::wostringstream WriterBuffer;
    int32_t CurrentIndentionLevel{0};
    bool bNewlineIndentPlaced{false};
public:
    FormattedTextWriter& Append( const std::wstring& InString );
    FormattedTextWriter& Append( const wchar_t* InLiteral );

    FormattedTextWriter& AppendNewline( const std::wstring& InString );
    FormattedTextWriter& AppendNewline( const wchar_t* InLiteral );

    FormattedTextWriter& AppendFormat( const wchar_t* InFormat, ... );
    FormattedTextWriter& AppendFormatNewline( const wchar_t* InFormat, ... );
    FormattedTextWriter& AppendFormatNewlineNoIndent( const wchar_t* InFormat, ... );
    FormattedTextWriter& AppendNewline();

    void PlaceNewlineIndent( int32_t IndentLevelsToIgnore = 0 );
    void ResetNewlineIndent();

    void IncrementIndentionLevel();
    void DecrementIndentionLevel();

    void WriteToFile( const std::filesystem::path& FilePath ) const;
    std::wstring ToString() const;
};

class ScopedIndentionLevel
{
    FormattedTextWriter& Writer;
public:
    explicit ScopedIndentionLevel( FormattedTextWriter& InWriter ) : Writer( InWriter )
    {
        Writer.IncrementIndentionLevel();
    }
    ~ScopedIndentionLevel()
    {
        Writer.DecrementIndentionLevel();
    }
};