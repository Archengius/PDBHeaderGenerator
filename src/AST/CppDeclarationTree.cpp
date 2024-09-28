#include "AST/CppDeclarationTree.h"
#include <assert.h>
#include <fstream>
#include <iostream>
#include "Utils/TextWriter.h"
#include "Utils/TopoSort.h"

CppFile::CppFile(const DataModel::EDataModel InDataModel, const uint32_t InTypeFormatFlags)
{
    FormatRules.BaseTypeFormatRules = TypeFormattingRules{ InDataModel, InTypeFormatFlags };
}

void CppFile::Print(FormattedTextWriter& TextWriter) const
{
    if ( bIsHeaderFile )
    {
        TextWriter.AppendNewline(L"#pragma once").AppendNewline();
    }
    else
    {
        TextWriter.AppendFormatNewline(L"#include \"%s.h\"", FileName.c_str());
    }

    for ( const std::wstring& LocalInclude : LocalIncludes )
    {
        TextWriter.AppendFormatNewline(L"#include \"%s.h\"", LocalInclude.c_str());
    }
    if ( !LocalIncludes.empty() )
    {
        TextWriter.AppendNewline();
    }

    for ( const std::wstring& SystemInclude : SystemIncludes )
    {
        TextWriter.AppendFormatNewline(L"#include <%s>", SystemInclude.c_str());
    }
    if ( !SystemIncludes.empty() )
    {
        TextWriter.AppendNewline();
    }

    // Sort top level declarations based on their relative dependencies
    std::vector<std::shared_ptr<ITopLevelDeclaration>> DeclarationsCopy = Declarations;

    // Sort top level declarations based on their dependencies
    topological_sort( DeclarationsCopy.begin(), DeclarationsCopy.end(), [](const std::shared_ptr<ITopLevelDeclaration>& From, const std::shared_ptr<ITopLevelDeclaration>& To)
    {
        return To->Dependencies.contains(From.get());
    } );

    // Generate top level declarations inside of their relevant namespaces
    std::wstring CurrentNamespace = L"";
    const wchar_t* LastElementType = nullptr;

    for ( const std::shared_ptr<ITopLevelDeclaration>& Declaration : DeclarationsCopy )
    {
        bool bNewlineAlreadyAppended = false;
        // If the current namespace does not match, close it
        if ( Declaration->Namespace != CurrentNamespace )
        {
            // Close the current namespace if we have one open
            if ( !CurrentNamespace.empty() )
            {
                TextWriter.DecrementIndentionLevel();
                TextWriter.AppendNewline(L"}");
                TextWriter.AppendNewline();
                bNewlineAlreadyAppended = true;
            }
            CurrentNamespace.clear();
        }

        // If this declaration is not inline, or the type of the previous declaration is different, we want to emit an additional newline
        const wchar_t* CurrentElementType = Declaration->GetDeclarationTypeName();
        if ( LastElementType == nullptr || wcscmp( LastElementType, CurrentElementType ) != 0 || !Declaration->IsInlineDeclaration() )
        {
            // Only append newline if we are not the first declaration in the file
            if ( LastElementType != nullptr && !bNewlineAlreadyAppended )
            {
                TextWriter.AppendNewline();
            }
            LastElementType = CurrentElementType;
        }

        // If current namespace does not match, open a new one
        if ( Declaration->Namespace != CurrentNamespace )
        {
            CurrentNamespace = Declaration->Namespace;

            // Open a new namespace
            if ( !CurrentNamespace.empty() )
            {
                TextWriter.AppendFormatNewline(L"namespace %s", CurrentNamespace.c_str());
                TextWriter.AppendNewline(L"{");
                TextWriter.IncrementIndentionLevel();
            }
        }
        Declaration->Print( TextWriter, FormatRules.AppendScope( CurrentNamespace ) );
    }

    // Close the final namespace if we have one open
    if ( !CurrentNamespace.empty() )
    {
        TextWriter.DecrementIndentionLevel();
        TextWriter.AppendNewline(L"}");
        TextWriter.AppendNewline();
    }
}

namespace Foo
{

}

void CppFile::WriteToFile(const std::filesystem::path& DirectoryPath) const
{
    FormattedTextWriter TextWriter;
    Print( TextWriter );
    create_directories(DirectoryPath);

    std::filesystem::path Filename(FileName);
    Filename.replace_extension( std::filesystem::path( bIsHeaderFile ? L"h" : L"cpp" ) );
    const std::filesystem::path FinalFilePath = absolute( DirectoryPath / Filename );

    std::wcout << L"Writing file: " << FinalFilePath.generic_wstring() << std::endl;
    TextWriter.WriteToFile( FinalFilePath );
}

void TypedefDeclaration::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    TextWriter.AppendFormatNewline(L"typedef %s %s;", TypeName.c_str(), TypedefName.c_str());
}

void EnumDeclarationData::PrintEnum(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const bool bIsAnonymousEnum) const
{
    assert( UnderlyingType->GetId() == ETypeDeclarationId::FundamentalType && L"Enum underlying type should be a fundamental integral type" );
    const bool bIsUnsigned = static_cast<const FundamentalTypeDeclaration*>( UnderlyingType.get() )->bIsUnsigned;
    if ( !bIsAnonymousEnum )
    {

        TextWriter.AppendFormat(L"%s %s : ", bIsScoped ? L"enum class" : L"enum", EnumName.c_str());
        UnderlyingType->Print(TextWriter, Rules);
        TextWriter.AppendNewline();
    }
    else
    {
        TextWriter.Append(L"enum : ");
        UnderlyingType->Print(TextWriter, Rules);
        TextWriter.AppendNewline();
    }
    TextWriter.AppendNewline(L"{");
    TextWriter.IncrementIndentionLevel();

    for ( const auto& [ValueName, ValueInt] : Values )
    {
        TextWriter.AppendFormatNewline(bIsUnsigned ? L"%s = %llu," : L"%s = %lld,", ValueName.c_str(), ValueInt);
    }
    TextWriter.DecrementIndentionLevel();
    if ( !bIsAnonymousEnum )
    {
        TextWriter.AppendNewline(L"};");
    }
    else
    {
        TextWriter.Append(L"}");
    }
}

EnumDeclaration::EnumDeclaration(const std::shared_ptr<EnumDeclarationData>& InDeclData) : Data( InDeclData )
{
}

void EnumDeclaration::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    Data->PrintEnum(TextWriter, Rules.BaseTypeFormatRules);
}

UDTNestedEnumDeclaration::UDTNestedEnumDeclaration(const std::shared_ptr<EnumDeclarationData>& InDeclData) : Data( InDeclData )
{
}

void UDTNestedEnumDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    Data->PrintEnum(TextWriter, Rules);
}

PredeclarationStatement::PredeclarationStatement(const std::shared_ptr<ITypeDeclaration>& InTypeStatement) : PredeclarationType( InTypeStatement )
{
    // Extract namespace from the type to use it
    if ( InTypeStatement->GetId() == ETypeDeclarationId::Enum )
    {
        const EnumTypeDeclaration* EnumDeclaration = static_cast<const EnumTypeDeclaration*>( InTypeStatement.get() );
        Namespace = EnumDeclaration->OuterScope;
        assert( EnumDeclaration->OuterType == nullptr && L"Predeclarations cannot be nested" );
    }
    else if ( InTypeStatement->GetId() == ETypeDeclarationId::UDT )
    {
        const UDTTypeDeclaration* UDTDeclaration = static_cast<const UDTTypeDeclaration*>( InTypeStatement.get() );
        Namespace = UDTDeclaration->OuterScope;
        assert( UDTDeclaration->OuterType == nullptr && L"Predeclarations cannot be nested" );
    }
}

void PredeclarationStatement::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    // Predeclarations always need CSU/Enum specifiers. Essentially, they are type names with specifiers.
    // We also do not emit outer scope since it is handled by the namespace of the top level statement
    PredeclarationType->Print(TextWriter, Rules.BaseTypeFormatRules
        .AppendFlagsNonInheritable( TypeFormattingRules::EmitEnumSpecifier | TypeFormattingRules::EmitCSUSpecifier | TypeFormattingRules::SkipOuterScope ) );
    TextWriter.AppendNewline(L";");
}

const wchar_t* CppAccessModifierToString(const CppAccessModifier AccessModifier)
{
    switch ( AccessModifier )
    {
        case CppAccessModifier::Private: return L"private";
        case CppAccessModifier::Protected: return L"protected";
        case CppAccessModifier::Public: return L"public";
    }
    assert(0);
    return L"";
}

CppAccessModifier DefaultAccessModifierForCppUDTKind(const CppUDTKind Kind)
{
    switch ( Kind )
    {
        case CppUDTKind::Class: return CppAccessModifier::Private;
        case CppUDTKind::Struct: return CppAccessModifier::Public;
        case CppUDTKind::Union: return CppAccessModifier::Public;
    }
    assert(0);
    return CppAccessModifier::Private;
}

void UDTFunctionDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    if ( !Comment.empty() )
    {
        TextWriter.AppendFormatNewline( L"/* %s */", Comment.c_str() );
    }
    if ( bIsTemplateSpecialization )
    {
        TextWriter.Append(L"template<> ");
    }
    if ( !InlineImplementation.empty() )
    {
        TextWriter.Append(L"__forceinline ");
    }
    if ( bIsExplicit )
    {
        TextWriter.Append(L"explicit ");
    }
    if ( bIsStatic )
    {
        TextWriter.Append(L"static ");
    }
    else if ( bIsVirtual )
    {
        TextWriter.Append(L"virtual ");
    }

    if ( !bNoReturnType )
    {
        ReturnType->Print(TextWriter, Rules);
        TextWriter.Append(L" ");
    }
    TextWriter.AppendFormat(L"%s", MemberName.c_str());
    if ( bIsTemplateSpecialization )
    {
        TextWriter.Append(L"<");
        bool bIsFirstTemplateArgument = true;
        for ( const TypeTemplateArgument& Argument : TemplateArguments )
        {
            if ( !bIsFirstTemplateArgument )
            {
                TextWriter.Append(L", ");
            }
            bIsFirstTemplateArgument = false;
            Argument.Print( TextWriter, Rules );
        }
        TextWriter.Append(L">");
    }
    TextWriter.Append(L"(");

    bool bIsFirstParameter = true;
    for ( const auto& [ParameterName, ParameterType] : ParameterNamesAndTypes )
    {
        if ( !bIsFirstParameter )
        {
            TextWriter.Append(L", ");
        }
        bIsFirstParameter = false;
        if ( !ParameterName.empty() )
        {
            ParameterType->Print(TextWriter, Rules);
            TextWriter.AppendFormat(L" %s", ParameterName.c_str());
        }
        else
        {
            ParameterType->Print(TextWriter, Rules);
        }
    }
    if ( bIsVariadicArguments )
    {
        if ( !bIsFirstParameter )
        {
            TextWriter.Append(L", ");
        }
        TextWriter.Append(L"...");
    }
    TextWriter.Append(L")");
    if ( bIsConst )
    {
        TextWriter.Append(L" const");
    }
    if ( !bIsStatic && bIsVirtual )
    {
        if ( bIsOverride )
        {
            TextWriter.Append(L" override");
        }
        if ( bIsPureVirtual )
        {
            TextWriter.Append(L" = 0");
        }
    }
    if ( !InlineImplementation.empty() )
    {
        TextWriter.Append( InlineImplementation );
        TextWriter.AppendNewline();
    }
    else
    {
        TextWriter.AppendNewline(L";");
    }
}

void UDTDataMemberDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    if ( bIsStatic )
    {
        TextWriter.Append(L"static ");
    }
    if ( bIsConstexpr )
    {
        TextWriter.Append(L"constexpr ");
    }
    else if ( bIsConst )
    {
        TextWriter.Append(L"const ");
    }
    else if ( bIsThreadLocal )
    {
        TextWriter.Append(L"thread_local ");
    }

    // Print type and member name
    MemberType->PrintVariableType( TextWriter, Rules, MemberName );
    if ( BitfieldSize != -1 )
    {
        TextWriter.AppendFormat(L": %d", BitfieldSize);
    }

    if ( !ConstantValue.empty() )
    {
        assert( BitfieldSize == -1 ); // bitfields cannot have value assigned to them
        TextWriter.AppendFormat(L" = %s", ConstantValue.c_str());
    }
    else if ( bWantsDefaultInitializer )
    {
        assert( BitfieldSize == -1 ); // bitfields cannot have default initializers
        TextWriter.Append(L"{}");
    }
    TextWriter.AppendNewline(L";");
}

void UDTDeclarationBaseClass::Print(FormattedTextWriter& TextWriter, const CppUDTKind ClassKind, const TypeFormattingRules& Rules) const
{
    // Only emit access modifier if it is different from the default one
    if ( AccessModifier != DefaultAccessModifierForCppUDTKind( ClassKind ) )
    {
        TextWriter.AppendFormat(L"%s ", CppAccessModifierToString( AccessModifier ));
    }
    if ( bIsVirtual )
    {
        TextWriter.Append(L"virtual ");
    }
    assert( BaseClass->IsInlineDeclaration() && L"Base class type declarations should always be inline" );
    assert( BaseClass->GetId() == ETypeDeclarationId::UDT && L"Base class type should be an UDT type declaration" );
    BaseClass->Print(TextWriter, Rules);
}

void UDTDeclarationData::PrintUDT(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const bool bIsAnonymousType ) const
{
    if ( !bIsAnonymousType )
    {
        if ( bIsTemplateSpecialization )
        {
            TextWriter.Append(L"template<> ");
        }
        TextWriter.AppendFormat(L"%s ", CppUDTKindToString(Kind));

        if ( bIsDllImport )
        {
            TextWriter.Append(L"DLLIMPORT ");
        }
        TextWriter.AppendFormat(L"%s", ClassName.c_str());

        if ( bIsTemplateSpecialization )
        {
            TextWriter.Append(L"<");
            bool bIsFirstTemplateArgument = true;
            for ( const TypeTemplateArgument& Argument : TemplateArguments )
            {
                if ( !bIsFirstTemplateArgument )
                {
                    TextWriter.Append(L", ");
                }
                bIsFirstTemplateArgument = false;
                Argument.Print( TextWriter, Rules );
            }
            TextWriter.Append(L">");
        }

        if ( bIsFinal )
        {
            TextWriter.Append(L" final");
        }
        if ( !BaseClasses.empty() )
        {
            TextWriter.Append(L" : ");
            bool bIsFirstBaseClass = true;
            for ( const UDTDeclarationBaseClass& BaseClass : BaseClasses )
            {
                if ( !bIsFirstBaseClass )
                {
                    TextWriter.Append(L", ");
                }
                bIsFirstBaseClass = false;
                BaseClass.Print( TextWriter, Kind, Rules );
            }
        }
    }
    else
    {
        TextWriter.Append( CppUDTKindToString(Kind) );
    }
    TextWriter.AppendNewline();
    TextWriter.AppendNewline(L"{");
    TextWriter.IncrementIndentionLevel();

    // Sort top level declarations based on their dependencies
    std::vector<std::shared_ptr<IUDTDeclarationMember>> MembersCopy = Members;
    topological_sort( MembersCopy.begin(), MembersCopy.end(), [](const std::shared_ptr<IUDTDeclarationMember>& From, const std::shared_ptr<IUDTDeclarationMember>& To)
    {
        if ( To->Dependencies.contains(From.get()) )
        {
            return true;
        }
        if ( From->Priority > To->Priority )
        {
            return true;
        }
        return false;
    } );

    CppAccessModifier CurrentAccessModifier = DefaultAccessModifierForCppUDTKind( Kind );
    for ( const std::shared_ptr<IUDTDeclarationMember>& MemberDeclaration : MembersCopy )
    {
        // Update access modifier if it does not match the current one
        if ( MemberDeclaration->AccessModifier != CurrentAccessModifier )
        {
            TextWriter.AppendFormatNewlineNoIndent(L"%s:", CppAccessModifierToString(MemberDeclaration->AccessModifier));
            CurrentAccessModifier = MemberDeclaration->AccessModifier;
        }
        MemberDeclaration->Print(TextWriter, bIsAnonymousType ? Rules : Rules.AppendScope(ClassName) );
    }

    TextWriter.DecrementIndentionLevel();
    if ( !bIsAnonymousType )
    {
        TextWriter.AppendNewline(L"};");
    }
    else
    {
        TextWriter.Append(L"}");
    }
}

UDTNestedTypeDeclaration::UDTNestedTypeDeclaration(const std::shared_ptr<UDTDeclarationData>& InDeclData) : Data( InDeclData )
{
}

UDTDeclaration::UDTDeclaration(const std::shared_ptr<UDTDeclarationData>& InDeclData) : Data( InDeclData )
{
}

void UDTNestedTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    Data->PrintUDT(TextWriter, Rules);
}

void UDTDeclaration::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    Data->PrintUDT(TextWriter, Rules.BaseTypeFormatRules);
}

void GlobalDataDeclaration::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    // Make sure we follow the name mangling scheme used by the symbol, otherwise we will not be able to resolve imports
    if ( bIsExternCLinkage )
    {
        assert( !bIsTemplateSpecialization );
        TextWriter.Append(L"extern \"C\" ");
    }
    if ( bIsTemplateSpecialization )
    {
        TextWriter.Append(L"template<> ");
    }
    if ( bIsThreadLocal )
    {
        TextWriter.Append(L"thread_local ");
    }
    if ( bIsConstexpr )
    {
        TextWriter.Append(L"constexpr ");
    }
    if ( bIsDllImport )
    {
        TextWriter.Append(L"DLLIMPORT ");
    }

    // Print type and member name
    DataType->PrintVariableType( TextWriter, Rules.BaseTypeFormatRules, DataName );

    if ( bIsTemplateSpecialization )
    {
        TextWriter.Append(L"<");
        bool bIsFirstTemplateArgument = true;
        for ( const TypeTemplateArgument& Argument : TemplateArguments )
        {
            if ( !bIsFirstTemplateArgument )
            {
                TextWriter.Append(L", ");
            }
            bIsFirstTemplateArgument = false;
            Argument.Print( TextWriter, Rules.BaseTypeFormatRules );
        }
        TextWriter.Append(L">");
    }
    if ( !ConstantValue.empty() )
    {
        TextWriter.AppendFormat(L" = %s", ConstantValue.c_str());
    }
    TextWriter.AppendNewline(L";");
}

void GlobalFunctionDeclaration::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    // Make sure we follow the name mangling scheme used by the symbol, otherwise we will not be able to resolve imports
    if ( bIsExternCLinkage )
    {
        assert( !bIsTemplateSpecialization );
        TextWriter.Append(L"extern \"C\" ");
    }
    else if ( bIsDllImport )
    {
        TextWriter.Append(L"extern ");
    }

    if ( bIsTemplateSpecialization )
    {
        TextWriter.Append(L"template<> ");
    }
    if ( bIsDllImport )
    {
        TextWriter.Append(L"__declspec(dllimport) ");
    }
    ReturnType->Print(TextWriter, Rules.BaseTypeFormatRules);
    TextWriter.AppendFormat(L" %s", FunctionName.c_str());

    if ( bIsTemplateSpecialization )
    {
        TextWriter.Append(L"<");
        bool bIsFirstTemplateArgument = true;
        for ( const TypeTemplateArgument& Argument : TemplateArguments )
        {
            if ( !bIsFirstTemplateArgument )
            {
                TextWriter.Append(L", ");
            }
            bIsFirstTemplateArgument = false;
            Argument.Print( TextWriter, Rules.BaseTypeFormatRules );
        }
        TextWriter.Append(L">");
    }
    TextWriter.Append(L"(");

    bool bIsFirstParameter = true;
    for ( const auto& [ParameterName, ParameterType] : ParameterNamesAndTypes )
    {
        if ( !bIsFirstParameter )
        {
            TextWriter.Append(L", ");
        }
        bIsFirstParameter = false;
        if ( !ParameterName.empty() )
        {
            ParameterType->PrintVariableType(TextWriter, Rules.BaseTypeFormatRules, ParameterName);
        }
        else
        {
            ParameterType->Print(TextWriter, Rules.BaseTypeFormatRules);
        }
    }
    if ( bIsVariadicArguments )
    {
        if ( !bIsFirstParameter )
        {
            TextWriter.Append(L", ");
        }
        TextWriter.Append(L"...");
    }
    TextWriter.Append(L")");
    TextWriter.AppendNewline(L";");
}

void AnonymousUDTTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    Data->PrintUDT(TextWriter, Rules, true);
}

bool AnonymousUDTTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    // Two anonymous declarations are only identical if they are the same object
    return this == InOtherDeclaration.get();
}

std::shared_ptr<ITypeDeclaration> AnonymousUDTTypeDeclaration::Clone() const
{
    return std::make_shared<AnonymousUDTTypeDeclaration>( *this );
}

std::shared_ptr<ITypeDeclaration> AnonymousEnumTypeDeclaration::Clone() const
{
    return std::make_shared<AnonymousEnumTypeDeclaration>( *this );
}

size_t AnonymousUDTTypeDeclaration::GetDeclarationHash() const
{
    return std::hash<const void*>()( this );
}

void AnonymousEnumTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    Data->PrintEnum(TextWriter, Rules, true);
}

bool AnonymousEnumTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    // Two anonymous declarations are only identical if they are the same object
    return this == InOtherDeclaration.get();
}

size_t AnonymousEnumTypeDeclaration::GetDeclarationHash() const
{
    return std::hash<const void*>()( this );
}

void TemplateDeclarationArgument::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    if ( Type == ETemplateDeclarationArgumentType::Typename )
    {
        TextWriter.Append(L"typename");
    }
    else if ( Type == ETemplateDeclarationArgumentType::TypeValue )
    {
        TypeValueKind->Print(TextWriter, Rules.BaseTypeFormatRules);
    }
}

void TemplateTypeDeclaration::Print(FormattedTextWriter& TextWriter, const DeclarationPrintRules& Rules) const
{
    TextWriter.Append(L"template");
    TextWriter.Append(L"<");
    bool bIsFirstArgument = true;
    for ( const TemplateDeclarationArgument& Argument : Arguments )
    {
        if ( !bIsFirstArgument )
        {
            TextWriter.Append(L", ");
        }
        bIsFirstArgument = false;
        Argument.Print( TextWriter, Rules );
    }
    TextWriter.Append(L">");
    TextWriter.Append(L" ");

    TextWriter.AppendFormatNewline(L"%s %s;", CppUDTKindToString(UDTKind), ClassName.c_str());
}
