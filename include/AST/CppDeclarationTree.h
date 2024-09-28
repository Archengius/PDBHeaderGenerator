#pragma once

#include <cstdint>
#include <filesystem>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>
#include "CppTypeDeclaration.h"

class FormattedTextWriter;

struct DeclarationPrintRules
{
    // Base rules for type formatting
    TypeFormattingRules BaseTypeFormatRules;

    DeclarationPrintRules AppendScope( const std::wstring& InScopeElement ) const
    {
        DeclarationPrintRules NewRules;
        NewRules.BaseTypeFormatRules = BaseTypeFormatRules.AppendScope( InScopeElement );
        return NewRules;
    }
};

class ITopLevelDeclaration
{
public:
    // Namespace in which declaration resides. Declarations in the same namespace can share the namespace block
    std::wstring Namespace;
    // Top level declarations that this declaration depends on. Used to sort the dependencies.
    std::unordered_set<ITopLevelDeclaration*> Dependencies;
    std::wstring Comment;

    virtual ~ITopLevelDeclaration() = default;
    virtual void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const = 0;
    virtual bool IsInlineDeclaration() const { return true; }
    virtual const wchar_t* GetDeclarationTypeName() = 0;
};

class CppFile final
{
public:
    std::wstring FileName;
    bool bIsHeaderFile{false};
    std::vector<std::wstring> SystemIncludes;
    std::vector<std::wstring> LocalIncludes;
    std::vector<std::shared_ptr<ITopLevelDeclaration>> Declarations;
    DeclarationPrintRules FormatRules;

    explicit CppFile( DataModel::EDataModel InDataModel, uint32_t InTypeFormatFlags );
    void Print(FormattedTextWriter& TextWriter) const;
    void WriteToFile(const std::filesystem::path& DirectoryPath) const;
};

class TypedefDeclaration final : public ITopLevelDeclaration
{
public:
    std::wstring TypeName;
    std::wstring TypedefName;

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const override;
    const wchar_t* GetDeclarationTypeName() override { return L"typedef"; }
};

class PredeclarationStatement final : public ITopLevelDeclaration
{
    std::shared_ptr<ITypeDeclaration> PredeclarationType;
public:
    explicit PredeclarationStatement( const std::shared_ptr<ITypeDeclaration>& InTypeStatement );

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const override;
    const wchar_t* GetDeclarationTypeName() override { return L"predeclaration"; }
};

enum class CppAccessModifier : uint8_t
{
    Private,
    Protected,
    Public
};

const wchar_t* CppAccessModifierToString( CppAccessModifier AccessModifier );
CppAccessModifier DefaultAccessModifierForCppUDTKind( CppUDTKind Kind );

struct UDTDeclarationBaseClass
{
    std::shared_ptr<ITypeDeclaration> BaseClass;
    CppAccessModifier AccessModifier{};
    bool bIsVirtual{false};

    void Print(FormattedTextWriter& TextWriter, CppUDTKind ClassKind, const TypeFormattingRules& Rules) const;
};

class IUDTDeclarationMember
{
public:
    std::wstring MemberName;
    CppAccessModifier AccessModifier{};
    std::unordered_set<IUDTDeclarationMember*> Dependencies;
    int32_t Priority{0};
    std::wstring Comment;

    virtual ~IUDTDeclarationMember() = default;
    virtual void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const = 0;
};

struct UDTDeclarationData;
struct EnumDeclarationData;

class UDTDataMemberDeclaration final : public IUDTDeclarationMember
{
public:
    std::shared_ptr<ITypeDeclaration> MemberType;
    int32_t BitfieldSize{-1};
    bool bIsStatic{false};
    bool bIsConst{false};
    bool bIsThreadLocal{false};
    std::wstring ConstantValue;
    bool bIsTemplateSpecialization{false};
    std::vector<TypeTemplateArgument> TemplateArguments;
    bool bIsConstexpr{false};
    bool bWantsDefaultInitializer{false};

    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
};

class UDTFunctionDeclaration final : public IUDTDeclarationMember
{
public:
    std::shared_ptr<ITypeDeclaration> ReturnType;
    std::vector<std::pair<std::wstring, std::shared_ptr<ITypeDeclaration>>> ParameterNamesAndTypes;
    bool bIsVirtual{false};
    bool bIsConst{false};
    bool bIsPureVirtual{false};
    bool bIsOverride{false};
    bool bIsStatic{false};
    bool bNoReturnType{false};
    bool bIsExplicit{false};
    bool bIsTemplateSpecialization{false};
    std::vector<TypeTemplateArgument> TemplateArguments;
    bool bIsVariadicArguments{false};
    std::wstring InlineImplementation;

    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
};

struct UDTDeclarationData
{
    CppUDTKind Kind{};
    std::wstring ClassName;
    bool bIsFinal{false};
    int32_t AlignmentModifier{0};
    std::vector<UDTDeclarationBaseClass> BaseClasses;
    std::vector<std::shared_ptr<IUDTDeclarationMember>> Members;
    bool bIsTemplateSpecialization{false};
    std::vector<TypeTemplateArgument> TemplateArguments;
    bool bIsDllImport{false};

    void PrintUDT(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, bool bIsAnonymousType = false) const;
};

struct EnumDeclarationData
{
    std::wstring EnumName;
    std::shared_ptr<ITypeDeclaration> UnderlyingType;
    bool bIsScoped{false};
    // If the enumeration is signed, values will be interpreted as signed int64
    std::vector<std::pair<std::wstring, uint64_t>> Values;

    void PrintEnum(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, bool bIsAnonymousEnum = false) const;
};

class UDTNestedTypeDeclaration final : public IUDTDeclarationMember
{
    std::shared_ptr<UDTDeclarationData> Data;
public:
    explicit UDTNestedTypeDeclaration( const std::shared_ptr<UDTDeclarationData>& InDeclData );
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
};

class UDTNestedEnumDeclaration final : public IUDTDeclarationMember
{
    std::shared_ptr<EnumDeclarationData> Data;
public:
    explicit UDTNestedEnumDeclaration( const std::shared_ptr<EnumDeclarationData>& InDeclData );
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
};

class UDTDeclaration final : public ITopLevelDeclaration
{
    std::shared_ptr<UDTDeclarationData> Data;
public:
    explicit UDTDeclaration( const std::shared_ptr<UDTDeclarationData>& InDeclData );

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const override;
    bool IsInlineDeclaration() const override { return false; }
    const wchar_t* GetDeclarationTypeName() override { return L"type"; }
};

class EnumDeclaration final : public ITopLevelDeclaration
{
    std::shared_ptr<EnumDeclarationData> Data;
public:
    explicit EnumDeclaration( const std::shared_ptr<EnumDeclarationData>& InDeclData );

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const override;
    bool IsInlineDeclaration() const override { return false; }
    const wchar_t* GetDeclarationTypeName() override { return L"enum"; }
};

class GlobalDataDeclaration final : public ITopLevelDeclaration
{
public:
    std::wstring DataName;
    std::shared_ptr<ITypeDeclaration> DataType;
    bool bIsExternCLinkage{false};
    bool bIsThreadLocal{false};
    std::wstring ConstantValue;
    bool bIsTemplateSpecialization{false};
    std::vector<TypeTemplateArgument> TemplateArguments;
    bool bIsConstexpr{false};
    bool bIsDllImport{false};

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const override;
    const wchar_t* GetDeclarationTypeName() override { return L"data"; }
};

class GlobalFunctionDeclaration final : public ITopLevelDeclaration
{
public:
    std::wstring FunctionName;
    std::shared_ptr<ITypeDeclaration> ReturnType;
    std::vector<std::pair<std::wstring, std::shared_ptr<ITypeDeclaration>>> ParameterNamesAndTypes;
    bool bIsExternCLinkage{false};
    bool bIsTemplateSpecialization{false};
    std::vector<TypeTemplateArgument> TemplateArguments;
    bool bIsVariadicArguments{false};
    bool bIsDllImport{false};

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const override;
    const wchar_t* GetDeclarationTypeName() override { return L"function"; }
};

class AnonymousUDTTypeDeclaration final : public ITypeDeclaration
{
public:
    std::shared_ptr<UDTDeclarationData> Data;
    bool bIsConst{false};

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::AnonymousUDT; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    bool IsInlineDeclaration() const override { return false; }
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

class AnonymousEnumTypeDeclaration final : public ITypeDeclaration
{
public:
    std::shared_ptr<EnumDeclarationData> Data;
    bool bIsConst{false};

    ETypeDeclarationId GetId() const override { return ETypeDeclarationId::AnonymousEnum; }
    void Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const override;
    bool Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const override;
    bool IsInlineDeclaration() const override { return false; }
    std::shared_ptr<ITypeDeclaration> Clone() const override;
    size_t GetDeclarationHash() const override;
};

enum class ETemplateDeclarationArgumentType
{
    Typename,
    TypeValue,
};

struct TemplateDeclarationArgument
{
    ETemplateDeclarationArgumentType Type{};
    std::shared_ptr<ITypeDeclaration> TypeValueKind;

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const;
};

class TemplateTypeDeclaration final : public ITopLevelDeclaration
{
public:
    std::vector<TemplateDeclarationArgument> Arguments;
    CppUDTKind UDTKind{};
    std::wstring ClassName;

    void Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const override;
    const wchar_t* GetDeclarationTypeName() override { return L"template"; }
};
