#pragma once

#include <assert.h>
#include <cstdint>
#include <xstring>
#include <memory>
#include <optional>
#include <unordered_map>
#include <vector>

class FormattedTextWriter;

namespace DataModel
{
    enum EDataModel : uint8_t
    {
        // int is 32 bit, long is 32 bit, pointer is 64 bit. Used by x64 Windows
        LLP64
    };
    struct DataModelDefinition
    {
        int32_t CharSizeBytes{};
        int32_t ShortIntSizeBytes{};
        int32_t IntSizeBytes{};
        int32_t LongIntSizeBytes{};
        int32_t LongLongIntSizeBytes{};
        int32_t PointerSizeBytes{};
    };

    // Returns the data model definition for the given model enum
    DataModelDefinition GetDataModelDefinition( EDataModel InDataModel );
}

class TypeFormattingRules
{
    uint32_t Flags{0};
    uint32_t AdditionalFlagsNonInheritable{0};
    uint32_t FlagsMaskNonInheritable{0};
    DataModel::EDataModel DataModel{DataModel::LLP64};
    std::wstring CurrentScope;
public:
    enum ETypeFormattingFlags : uint32_t
    {
        None = 0x00000000,
        // Int has a fixed length
        FixedLengthInt = 0x00000001,
        // Print fixed length integer types (MS specific types and all ints if FixedLengthInt is used) using cstdint defines, such as int64_t
        Cstdint = 0x00000002,
        // Emit CSU specifiers when emitting type
        EmitCSUSpecifier = 0x00000004,
        // Enum enum specifier when emitting type
        EmitEnumSpecifier = 0x00000008,
        // Do not emit outer scope and type
        SkipOuterScope = 0x00000010,
        // Emit function calling conventions
        EmitCallingConvention = 0x00000020,
    };

    TypeFormattingRules() = default;
    TypeFormattingRules( const DataModel::EDataModel InDataModel, const uint32_t InFlags ) : Flags( InFlags ), DataModel( InDataModel ) {}

    DataModel::EDataModel GetDataModel() const { return DataModel; }

    bool HasAnyFlags(const uint32_t InFlags ) const
    {
        return (( Flags & ~FlagsMaskNonInheritable | AdditionalFlagsNonInheritable ) & InFlags) != 0;
    }
    bool HasAllFlags(const uint32_t InFlags ) const
    {
        return (( Flags & ~FlagsMaskNonInheritable | AdditionalFlagsNonInheritable ) & InFlags) == InFlags;
    }

    TypeFormattingRules AppendScope( const std::wstring& InScope ) const
    {
        TypeFormattingRules NewRules = *this;
        if ( !NewRules.CurrentScope.empty() && !InScope.empty() )
        {
            NewRules.CurrentScope.append(L"::");
        }
        NewRules.CurrentScope.append( InScope );
        return NewRules;
    }
    std::wstring GetRelativeScope( const std::wstring& InScope ) const
    {
        int32_t Offset1 = 0;
        int32_t Offset2 = 0;
        while ( true )
        {
            const int32_t NewOffset1 = CurrentScope.find( L"::", Offset1 );
            const int32_t NewOffset2 = InScope.find( L"::", Offset2 );

            const std::wstring_view Segment1( &CurrentScope[Offset1], ( NewOffset1 == std::wstring::npos ? CurrentScope.size() : NewOffset1 ) - Offset1 );
            const std::wstring_view Segment2( &InScope[Offset2], ( NewOffset2 == std::wstring::npos ? InScope.size() : NewOffset2 ) - Offset2 );

            // Paths were the same before but are now different
            if ( Segment1 != Segment2 )
            {
                return InScope.substr(Offset1);
            }
            // There is no segments left in the target scope. The relative scope is empty.
            if ( NewOffset2 == std::wstring::npos )
            {
                return std::wstring();
            }
            // There is no segments left in the current scope, take the remainder of the path
            if ( NewOffset1 == std::wstring::npos )
            {
                return InScope.substr( NewOffset2 + 2 );
            }
            // Path segments are still matching, update the offsets
            Offset1 = NewOffset1 + 2;
            Offset2 = NewOffset2 + 2;
        }
    }

    // Filters out non-inheritable flags
    TypeFormattingRules InheritableFlags() const
    {
        return TypeFormattingRules{ DataModel, Flags };
    }

    // Excludes (inheritable) flags
    TypeFormattingRules ExcludeFlags( const uint32_t InFlags ) const
    {
        TypeFormattingRules NewRules = *this;
        NewRules.Flags &= ~InFlags;
        return NewRules;
    }
    // Includes (inheritable) flags
    TypeFormattingRules IncludeFlags( const uint32_t InFlags ) const
    {
        TypeFormattingRules NewRules = *this;
        NewRules.Flags |= InFlags;
        return NewRules;
    }
    // Explicitly excludes specific flags, even if they are set as inheritable. Only works for the current type, not inherited by nested type declarations.
    TypeFormattingRules MaskFlagsNonInheritable( const uint32_t InFlags ) const
    {
        TypeFormattingRules NewRules = *this;
        NewRules.FlagsMaskNonInheritable |= InFlags;
        NewRules.AdditionalFlagsNonInheritable &= ~NewRules.FlagsMaskNonInheritable;
        return NewRules;
    }
    // Sets specific flags for the current type without modifying inherited flags. Only works for the current type, not inherited by nested type declarations.
    TypeFormattingRules AppendFlagsNonInheritable( const uint32_t InFlags ) const
    {
        TypeFormattingRules NewRules = *this;
        NewRules.AdditionalFlagsNonInheritable |= InFlags;
        NewRules.FlagsMaskNonInheritable &= ~NewRules.AdditionalFlagsNonInheritable;
        return NewRules;
    }
};

enum class CppUDTKind : uint8_t
{
    Class,
    Struct,
    Union
};

const wchar_t* CppUDTKindToString( CppUDTKind Kind );

enum class ETypeDeclarationId : uint32_t
{
    Generic,
    PointerType,
    FundamentalType,
    VoidType,
    FunctionType,
    UDT,
    Enum,
    AnonymousUDT,
    AnonymousEnum,
    ArrayType,
    InternalType,
    WildcardType,
};

class ITypeDeclaration;
struct TypeMemberReference;

class TypeDeclarationMatchContext
{
    std::unordered_map<int32_t, std::shared_ptr<ITypeDeclaration>> WildcardMatches;
    std::unordered_map<int32_t, int64_t> IntegerWildcardMatches;
    std::unordered_map<int32_t, TypeMemberReference> TypeMemberReferenceWildcardMatches;
public:
    bool MatchWildcard( int32_t InWildcardIndex, bool bIsWildcardConst, const std::shared_ptr<ITypeDeclaration>& InMatchAgainst );
    bool MatchIntegerWildcard( int32_t InWildcardIndex, int64_t InIntegerConstant );
    bool MatchTypeMemberReferenceWildcard( int32_t InWildcardIndex, const TypeMemberReference& InTypeMemberReference );

    std::shared_ptr<ITypeDeclaration> SubstituteWildcard( int32_t InWildcardIndex, bool bIsWildcardConst ) const;
    int64_t SubstituteIntegerWildcard( int32_t InWildcardIndex ) const;
    TypeMemberReference SubstituteTypeMemberReferenceWildcard( int32_t InWildcardIndex ) const;
};

struct TypeMemberReference
{
    std::shared_ptr<ITypeDeclaration> OwnerType;
    std::wstring MemberName;

    friend bool operator==(const TypeMemberReference& A, const TypeMemberReference& B);
    bool Match(const TypeMemberReference& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const;
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const;
    TypeMemberReference Substitute(const TypeDeclarationMatchContext& MatchContext) const;
    TypeMemberReference Clone() const;
    size_t GetTypeMemberReferenceHash() const;
};

class ITypeDeclaration : public std::enable_shared_from_this<ITypeDeclaration>
{
public:
    // Note: some types cannot be CV-qualified, in that case bIsConst value is ignored
    bool bIsConst{false};

    virtual ~ITypeDeclaration() = default;

    virtual ETypeDeclarationId GetId() const = 0;
    virtual void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const = 0;
    virtual bool IsInlineDeclaration() const { return true; }
    virtual bool Identical( const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration ) const = 0;
    virtual bool Match( const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext ) const;
    virtual std::shared_ptr<ITypeDeclaration> Clone() const = 0;
    virtual std::shared_ptr<ITypeDeclaration> Substitute( const TypeDeclarationMatchContext& MatchContext );
    virtual size_t GetDeclarationHash() const = 0;

    // Prints the type in addition to the variable name. Default implementation just appends the variable name to the type
    virtual void PrintVariableType(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::wstring& VariableName) const;

    // Parses type declaration from text
    static std::shared_ptr<ITypeDeclaration> ParseTypeDeclaration( const std::wstring& InText, bool bAllowWildcardTypes = false );

    static bool StaticIdentical( const std::shared_ptr<ITypeDeclaration>& A, const std::shared_ptr<ITypeDeclaration>& B );
    static bool StaticMatch( const std::shared_ptr<ITypeDeclaration>& Match, const std::shared_ptr<ITypeDeclaration>& MatchAgainst, TypeDeclarationMatchContext& MatchContext );
    static std::shared_ptr<ITypeDeclaration> StaticSubstitute( const std::shared_ptr<ITypeDeclaration>& Original, const TypeDeclarationMatchContext& MatchContext );
    static size_t StaticGetDeclarationHash( const std::shared_ptr<ITypeDeclaration>& Decl );
};

class PointerTypeDeclaration final : public ITypeDeclaration
{
public:
    std::shared_ptr<ITypeDeclaration> PointeeType;
    bool bIsReference{false};
    std::shared_ptr<ITypeDeclaration> OwnerType;

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::PointerType; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    bool Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const override;
    std::shared_ptr<ITypeDeclaration> Substitute(const TypeDeclarationMatchContext& MatchContext) override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

enum class EBasicType : int32_t
{
    Char, // length: 1 byte, can be signed or unsigned
    Int, // length: implementation defined & can be influenced by type modifiers. Possible values: 2, 4, 8 bytes. can be signed or unsigned
    Bool, // length: implementation defined, usually 1.
    WideChar, // length: implementation defined, 2 on windows, 4 on linux
    Char8, // length: implementation defined, at least 1
    Char16, // length: implementation defined, at least 2
    Char32, // length: implementation defined, at least 4
    Nullptr, // length: implementation defined, same as pointer to void
    Float, // length: implementation defined, usually 4 bytes
    Double, // length: implementatino defined, usually 8 bytes

    // Microsoft extension: Fixed length types __int64, __int32, __int16, __int8
    FixedLengthInt8, // length: 1 byte
    FixedLengthInt16, // length: 2 bytes
    FixedLengthInt32, // length: 4 bytes
    FixedLengthInt64, // length: 8 bytes
};

EBasicType GetFixedLengthIntType(int32_t InNumBytes);

class FundamentalTypeDeclaration final : public ITypeDeclaration
{
public:
    EBasicType BasicType{EBasicType::Char};
    bool bIsUnsigned{false};
    bool bIsShort{false};
    bool bIsLong{false};
    bool bIsLongLong{false};

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::FundamentalType; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

class VoidTypeDeclaration final : public ITypeDeclaration
{
public:
    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::VoidType; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

enum class ECallingConvention : uint8_t
{
    CDecl,
    StdCall,
    FastCall
};

class FunctionTypeDeclaration final : public ITypeDeclaration
{
public:
    std::shared_ptr<ITypeDeclaration> ReturnType;
    std::shared_ptr<ITypeDeclaration> OwnerType;
    std::vector<std::pair<std::wstring, std::shared_ptr<ITypeDeclaration>>> Arguments;
    bool bIsFunctionPointer{false};
    bool bIsConstMemberFunction{false};
    bool bIsVariadicArguments{false};
    std::optional<ECallingConvention> CallingConvention;

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::FunctionType; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    void PrintVariableType(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::wstring& VariableName) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    bool Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const override;
    std::shared_ptr<ITypeDeclaration> Substitute(const TypeDeclarationMatchContext& MatchContext) override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
private:
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::optional<std::wstring>& VariableName) const;
};

enum class ETemplateArgumentType : uint8_t
{
    None,
    IntegerConst,
    TypeDeclaration,
    TypeMemberReference,
    IntegerWildcard,
    TypeMemberReferenceWildcard,
};

struct TypeTemplateArgument
{
    ETemplateArgumentType Type{ETemplateArgumentType::None};
    int64_t IntegerConstant{0};
    std::shared_ptr<ITypeDeclaration> TypeConstant;
    TypeMemberReference TypeMemberReference;
    int32_t WildcardIndex{0};

    friend bool operator==(const TypeTemplateArgument& A, const TypeTemplateArgument& B);
    bool Match(const TypeTemplateArgument& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const;
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const;
    TypeTemplateArgument Substitute(const TypeDeclarationMatchContext& MatchContext) const;
    TypeTemplateArgument Clone() const;
    size_t GetTemplateArgumentHash() const;

    static bool ParseTemplateArguments(const std::wstring& InText, std::vector<TypeTemplateArgument>& OutArguments, bool bAllowWildcardTypes = false);
};

struct TypeTemplateArgumentContainer
{
    std::vector<TypeTemplateArgument> Arguments;

    friend bool operator==(const TypeTemplateArgumentContainer& A, const TypeTemplateArgumentContainer& B);
    bool Match(const TypeTemplateArgumentContainer& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const;
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const;
    TypeTemplateArgumentContainer Substitute(const TypeDeclarationMatchContext& MatchContext) const;
    TypeTemplateArgumentContainer Clone() const;
    size_t GetContainerHash() const;

    static bool ParseTemplateArguments( const std::wstring& InText, TypeTemplateArgumentContainer& ArgumentContainer, bool bAllowWildcardTypes = false );
};

// std::hash specialization for template arguments
template<> struct std::hash<TypeTemplateArgumentContainer>
{
    std::size_t operator()(const TypeTemplateArgumentContainer& Container) const noexcept
    {
        return Container.GetContainerHash();
    }
};

enum class EInternalIdentifier : uint8_t
{
    AnonymousTag,
    UnnamedTag,
    UnnamedType,
    UnnamedEnum,
    Lambda
};

class InternalTypeDeclaration final : public ITypeDeclaration
{
public:
    std::shared_ptr<ITypeDeclaration> OuterType;
    EInternalIdentifier Identifier{};
    std::wstring InternalTypeName;
    int64_t LambdaIndex{};

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::InternalType; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

class WildcardTypeDeclaration final : public ITypeDeclaration
{
public:
    int32_t WildcardIndex{};

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::WildcardType; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    bool Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const override;
    std::shared_ptr<ITypeDeclaration> Substitute(const TypeDeclarationMatchContext& MatchContext) override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

class UDTTypeDeclaration final : public ITypeDeclaration
{
public:
    // Format is: [OuterType::][OuterScope::]<ClassName>
    std::shared_ptr<ITypeDeclaration> OuterType;
    std::wstring OuterScope;
    std::wstring ClassName;
    TypeTemplateArgumentContainer TemplateArguments;
    bool bIsGlobalNamespace{false};
    // Data for pre-declaration
    std::optional<CppUDTKind> UDTKind;

    UDTTypeDeclaration() = default;
    explicit UDTTypeDeclaration( const std::wstring& InClassName ) : ClassName(InClassName) {}

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::UDT; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    bool Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const override;
    std::shared_ptr<ITypeDeclaration> Substitute(const TypeDeclarationMatchContext& MatchContext) override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

class EnumTypeDeclaration final : public ITypeDeclaration
{
public:
    // Format is: [OuterType::][OuterScope::]<EnumName>
    std::shared_ptr<ITypeDeclaration> OuterType;
    std::wstring OuterScope;
    std::wstring EnumName;
    bool bIsGlobalNamespace{false};
    // Data for pre-declaration
    std::optional<bool> bIsEnumClass;
    std::shared_ptr<ITypeDeclaration> UnderlyingType;

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::Enum; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    std::shared_ptr<ITypeDeclaration> Substitute(const TypeDeclarationMatchContext& MatchContext) override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

class ArrayTypeDeclaration final : public ITypeDeclaration
{
public:
    std::shared_ptr<ITypeDeclaration> ElementType;
    std::optional<int32_t> ArrayDimension;
    // Note: arrays cannot be const, only their elements can be

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::ArrayType; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    void PrintVariableType(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::wstring& VariableName) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    bool Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const override;
    std::shared_ptr<ITypeDeclaration> Substitute(const TypeDeclarationMatchContext& MatchContext) override;
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
private:
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::optional<std::wstring>& VariableName) const;
};

enum class ETypeTextToken : uint8_t
{
    EndOfLine,
    Identifier,
    InternalIdentifier,
    Integer,
    Float,
    ScopeDelimiter,
    Pointer,
    Reference,
    ArrayLBracket,
    ArrayRBracket,
    LBracket,
    RBracket,
    UDTKindSpecifier,
    EnumSpecifier,
    Comma,
    TemplateLBracket,
    TemplateRBracket,
    BaseClassDelimiter,
    TypeModifier,
    FundamentalType,
    Void,
    CallingConvention,
    VariadicArguments,
    // Extension syntax to the normal token set for partial template specialization matching
    TypeWildcard,
    IntegerWildcard,
    TypeMemberReferenceWildcard,
    Invalid
};

enum class ETypeModifier : uint8_t
{
    None,
    SignedModifier,
    UnsignedModifier,
    LongModifier,
    ShortModifier,
    Const
};

struct TypeTextToken
{
    ETypeTextToken Type{ETypeTextToken::Invalid};
    std::wstring_view Identifier{};
    int64_t IntegerValue{};
    double FloatValue{};
    CppUDTKind UdtKindValue{};
    ETypeModifier TypeModifier{ETypeModifier::None};
    EBasicType FundamentalType{EBasicType::Char};
    EInternalIdentifier InternalIdentifier{};
    std::wstring_view UnnamedEnumOrTypeVariableName;
    std::int64_t LambdaIndex{};
    ECallingConvention CallingConvention{};
    int32_t WildcardIndex{};
};

class FTypeTextParseHelper
{
    const std::wstring& RawText;
    int32_t Offset{0};
    bool bAllowWildcards{false};
public:
    FTypeTextParseHelper( const std::wstring& InRawText, const bool InAllowWildcards ) : RawText( InRawText ), bAllowWildcards( InAllowWildcards ) {}

    TypeTextToken PeekNextToken() const;
    TypeTextToken PeekNextNextToken() const;
    TypeTextToken ConsumeNextToken();
private:
    void SkipWhitespaces( int32_t& InOutOffset ) const;
    int32_t PeekNextTokenInternal( int32_t CurrentOffset, TypeTextToken& OutToken ) const;

    // Parses partial simple type declaration. Used for parsing nested type declarations when template instantiations are involved
    std::shared_ptr<ITypeDeclaration> ParsePartialSimpleDeclarationPartial( const std::shared_ptr<ITypeDeclaration>& OuterType, std::vector<ETypeModifier> AppliedTypeModifiers, bool bHasEnumSpecifier, bool bIsEnumClass, const std::optional<CppUDTKind>& CSUSpecifier );
    // This does not consume the first LBracket, you are expected to do it yourself
    std::shared_ptr<ITypeDeclaration> ParseFunctionPointerDeclaration( const std::shared_ptr<ITypeDeclaration>& ReturnType );
    // Parses type member name. Uses a simplified set of rules that are used for simple type declarations, and does not permit CSU or global namespaces
    TypeMemberReference ParseTypeMemberReference();
public:
    // Parses a complete type declaration
    std::shared_ptr<ITypeDeclaration> ParseCompleteTypeDeclaration();

    // Parses namespace and the type name from the token stream, like [::][ScopeName::][TypeName]
    bool ParseScopeAndTypeName( std::wstring& OutScopeName, std::wstring& OutTypeName, bool& OutIsGlobalNamespace );

    // Parses simple type declaration from the token stream. Simple type declaration is a type declaration consisting of an optional CSU specifier, a UDT, enum or fundamental type name, and optional const modifier
    std::shared_ptr<ITypeDeclaration> ParseSimpleTypeDeclaration();

    // Parses template argument list until it reaches the closing bracket or end of stream
    bool ParseTemplateArgumentsInternal( std::vector<TypeTemplateArgument>& OutTemplateArguments );
};
