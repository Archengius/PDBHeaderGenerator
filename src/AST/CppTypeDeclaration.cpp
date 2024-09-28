#include "AST/CppTypeDeclaration.h"
#include "Utils/TextWriter.h"
#include <algorithm>
#include <cassert>
#include <optional>
#include <ranges>
#include <Utils/StringUtils.h>

DataModel::DataModelDefinition DataModel::GetDataModelDefinition(const EDataModel InDataModel)
{
    if ( InDataModel == LLP64 )
    {
        static constexpr DataModelDefinition DataModelDefinition_LLP64{1, 2, 4, 4, 8, 8};
        return DataModelDefinition_LLP64;
    }
    assert(!L"Unnknown data model");
    return DataModelDefinition{};
}

const wchar_t* CppUDTKindToString(const CppUDTKind Kind)
{
    switch ( Kind )
    {
        case CppUDTKind::Class: return L"class";
        case CppUDTKind::Struct: return L"struct";
        case CppUDTKind::Union: return L"union";
    }
    assert(0);
    return L"";
}

EBasicType GetFixedLengthIntType(const int32_t InNumBytes)
{
    switch (InNumBytes)
    {
        case 1: return EBasicType::FixedLengthInt8;
        case 2: return EBasicType::FixedLengthInt16;
        case 4: return EBasicType::FixedLengthInt32;
        case 8: return EBasicType::FixedLengthInt64;
        default: ;
    }
    assert(!L"Unsupported fixed length size");
    return EBasicType::FixedLengthInt64;
}

static bool IsIntegralBasicType(const EBasicType InBasicType)
{
    switch (InBasicType)
    {
        case EBasicType::Char: return true;
        case EBasicType::Int: return true;
        case EBasicType::FixedLengthInt8: return true;
        case EBasicType::FixedLengthInt16: return true;
        case EBasicType::FixedLengthInt32: return true;
        case EBasicType::FixedLengthInt64: return true;
        default: return false;
    }
}

static const wchar_t* GetBasicTypeName(const EBasicType InBasicType)
{
    switch (InBasicType)
    {
        case EBasicType::Char: return L"char";
        case EBasicType::Int: return L"int";
        case EBasicType::Bool: return L"bool";
        case EBasicType::WideChar: return L"wchar_t";
        case EBasicType::Char8: return L"char8_t";
        case EBasicType::Char16: return L"char16_t";
        case EBasicType::Char32: return L"char32_t";
        case EBasicType::Nullptr: return L"nullptr_t";
        case EBasicType::Float: return L"float";
        case EBasicType::Double: return L"double";
        case EBasicType::FixedLengthInt8: return L"__int8";
        case EBasicType::FixedLengthInt16: return L"__int16";
        case EBasicType::FixedLengthInt32: return L"__int32";
        case EBasicType::FixedLengthInt64: return L"__int64";
    }
    assert(!L"Unsupported basic type");
    return L"<unknown>";
}

static const wchar_t* GetFixedLengthCstdintTypeName(const EBasicType InBasicType, bool bIsUnsigned)
{
    switch (InBasicType)
    {
        case EBasicType::FixedLengthInt8: return bIsUnsigned ? L"char" : L"char";
        case EBasicType::FixedLengthInt16: return bIsUnsigned ? L"uint16_t" : L"int16_t";
        case EBasicType::FixedLengthInt32: return bIsUnsigned ? L"uint32_t" : L"int32_t";
        case EBasicType::FixedLengthInt64: return bIsUnsigned ? L"uint64_t" : L"int64_t";
        default: return nullptr;
    }
}

void FundamentalTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    EBasicType ActualBasicType = BasicType;

    // Convert integer to fixed length value if we are asked to
    if ( BasicType == EBasicType::Int && Rules.HasAnyFlags( TypeFormattingRules::FixedLengthInt ) )
    {
        const DataModel::DataModelDefinition ModelDefinition = GetDataModelDefinition(Rules.GetDataModel());
        ActualBasicType = GetFixedLengthIntType( bIsLongLong ? ModelDefinition.LongLongIntSizeBytes : bIsLong ? ModelDefinition.LongIntSizeBytes : bIsShort ? ModelDefinition.ShortIntSizeBytes : ModelDefinition.IntSizeBytes );
    }
    // Convert char to fixed length (should be 1 byte everywhere)
    else if ( BasicType == EBasicType::Char && Rules.HasAnyFlags( TypeFormattingRules::FixedLengthInt ) )
    {
        const DataModel::DataModelDefinition ModelDefinition = GetDataModelDefinition(Rules.GetDataModel());
        ActualBasicType = GetFixedLengthIntType( ModelDefinition.CharSizeBytes );
    }

    // Check if we can print this type using cstdint.h
    const wchar_t* CStdintTypeName = nullptr;
    if ( IsIntegralBasicType(ActualBasicType) && Rules.HasAnyFlags( TypeFormattingRules::Cstdint ) )
    {
        CStdintTypeName = GetFixedLengthCstdintTypeName(ActualBasicType, bIsUnsigned);
    }

    // Emit const
    if ( bIsConst )
    {
        TextWriter.Append(L"const ");
    }
    // Emit unsigned if the basic type is integral and we are not emitting it as a part of the cstdint type
    if ( bIsUnsigned && IsIntegralBasicType( ActualBasicType ) && CStdintTypeName == nullptr )
    {
        TextWriter.Append(L"unsigned ");
    }
    // Emit type length modifiers if the basic type is int
    if ( ActualBasicType == EBasicType::Int && CStdintTypeName == nullptr )
    {
        TextWriter.Append(bIsLongLong ? L"long long " : bIsLong ? L"long " : bIsShort ? L"short " : L"");
    }
    // Emit the type name now
    TextWriter.Append(CStdintTypeName ? CStdintTypeName : GetBasicTypeName(ActualBasicType));
}

bool FundamentalTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    if ( InOtherDeclaration->GetId() == ETypeDeclarationId::FundamentalType )
    {
        const FundamentalTypeDeclaration& Other = static_cast<FundamentalTypeDeclaration&>( *InOtherDeclaration );
        if ( Other.BasicType != BasicType ) return false;
        if ( Other.bIsConst != bIsConst ) return false;
        if ( Other.bIsUnsigned != bIsUnsigned ) return false;
        return Other.bIsShort == bIsShort && Other.bIsLong == bIsLong && Other.bIsLongLong == bIsLongLong;
    }
    return false;
}

std::shared_ptr<ITypeDeclaration> FundamentalTypeDeclaration::Clone() const
{
    return std::make_shared<FundamentalTypeDeclaration>( *this );
}

size_t FundamentalTypeDeclaration::GetDeclarationHash() const
{
    size_t ResultHash = std::hash<ETypeDeclarationId>()( GetId() );
    HashCombine( ResultHash, BasicType );
    HashCombine( ResultHash, bIsConst );
    HashCombine( ResultHash, bIsUnsigned );
    HashCombine( ResultHash, bIsShort );
    HashCombine( ResultHash, bIsLong );
    HashCombine( ResultHash, bIsLongLong );
    return ResultHash;
}

void VoidTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    TextWriter.Append(L"void");
    if (bIsConst)
    {
        TextWriter.Append(L" ");
        TextWriter.Append(L" const");
    }
}

bool VoidTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    return InOtherDeclaration->GetId() == ETypeDeclarationId::VoidType;
}

std::shared_ptr<ITypeDeclaration> VoidTypeDeclaration::Clone() const
{
    return std::make_shared<VoidTypeDeclaration>();
}

size_t VoidTypeDeclaration::GetDeclarationHash() const
{
    return std::hash<ETypeDeclarationId>()( GetId() );
}

bool operator==(const TypeTemplateArgument& A, const TypeTemplateArgument& B)
{
    if ( A.Type != B.Type ) return false;
    switch (A.Type)
    {
        case ETemplateArgumentType::IntegerConst: return A.IntegerConstant == B.IntegerConstant;
        case ETemplateArgumentType::TypeDeclaration: return ITypeDeclaration::StaticIdentical(A.TypeConstant, B.TypeConstant);
        case ETemplateArgumentType::TypeMemberReference: return A.TypeMemberReference == B.TypeMemberReference;
        case ETemplateArgumentType::IntegerWildcard: return A.WildcardIndex == B.WildcardIndex;
        case ETemplateArgumentType::TypeMemberReferenceWildcard: return A.WildcardIndex == B.WildcardIndex;
        default: assert(!L"Failed to compare unknown type template argument type"); return false;
    }
}

bool TypeTemplateArgument::Match(const TypeTemplateArgument& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    if (Type == ETemplateArgumentType::IntegerConst && InMatchAgainst.Type == ETemplateArgumentType::IntegerConst)
    {
        return IntegerConstant == InMatchAgainst.IntegerConstant;
    }
    if (Type == ETemplateArgumentType::TypeDeclaration && InMatchAgainst.Type == ETemplateArgumentType::TypeDeclaration)
    {
        return ITypeDeclaration::StaticMatch( TypeConstant, InMatchAgainst.TypeConstant, MatchContext );
    }
    if (Type == ETemplateArgumentType::TypeMemberReference && InMatchAgainst.Type == ETemplateArgumentType::TypeMemberReference)
    {
        return TypeMemberReference == InMatchAgainst.TypeMemberReference;
    }
    if (Type == ETemplateArgumentType::IntegerWildcard && InMatchAgainst.Type == ETemplateArgumentType::IntegerConst)
    {
        return MatchContext.MatchIntegerWildcard( WildcardIndex, InMatchAgainst.IntegerConstant );
    }
    if (Type == ETemplateArgumentType::TypeMemberReferenceWildcard && InMatchAgainst.Type == ETemplateArgumentType::TypeMemberReference)
    {
        return MatchContext.MatchTypeMemberReferenceWildcard( WildcardIndex, InMatchAgainst.TypeMemberReference );
    }
    return false;
}

void TypeTemplateArgument::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    if (Type == ETemplateArgumentType::IntegerConst)
    {
        TextWriter.AppendFormat(L"%lld", IntegerConstant);
    }
    else if (Type == ETemplateArgumentType::TypeDeclaration)
    {
        TypeConstant->Print(TextWriter, Rules);
    }
    else if (Type == ETemplateArgumentType::TypeMemberReference)
    {
        TypeMemberReference.Print(TextWriter, Rules);
    }
    else if (Type == ETemplateArgumentType::IntegerWildcard)
    {
        TextWriter.AppendFormat(L"??%d", WildcardIndex);
    }
    else if (Type == ETemplateArgumentType::TypeMemberReferenceWildcard)
    {
        TextWriter.AppendFormat(L"?&%d", WildcardIndex);
    }
    else
    {
        assert(!L"Failed to print unknown type template argument type");
    }
}

TypeTemplateArgument TypeTemplateArgument::Substitute(const TypeDeclarationMatchContext& MatchContext) const
{
    if (Type == ETemplateArgumentType::IntegerConst)
    {
        TypeTemplateArgument Argument;
        Argument.Type = ETemplateArgumentType::IntegerConst;
        Argument.IntegerConstant = IntegerConstant;
        return Argument;
    }
    if (Type == ETemplateArgumentType::TypeDeclaration)
    {
        TypeTemplateArgument Argument;
        Argument.Type = ETemplateArgumentType::TypeDeclaration;
        Argument.TypeConstant = ITypeDeclaration::StaticSubstitute( TypeConstant, MatchContext );
        return Argument;
    }
    if (Type == ETemplateArgumentType::TypeMemberReference)
    {
        TypeTemplateArgument Argument;
        Argument.Type = ETemplateArgumentType::TypeMemberReference;
        Argument.TypeMemberReference = TypeMemberReference.Substitute( MatchContext );
        return Argument;
    }
    if (Type == ETemplateArgumentType::IntegerWildcard)
    {
        TypeTemplateArgument Argument;
        Argument.Type = ETemplateArgumentType::IntegerConst;
        Argument.IntegerConstant = MatchContext.SubstituteIntegerWildcard( WildcardIndex );
        return Argument;
    }
    if (Type == ETemplateArgumentType::TypeMemberReferenceWildcard)
    {
        TypeTemplateArgument Argument;
        Argument.Type = ETemplateArgumentType::TypeMemberReference;
        Argument.TypeMemberReference = MatchContext.SubstituteTypeMemberReferenceWildcard( WildcardIndex );
        return Argument;
    }

    assert(!L"Failed to print unknown type template argument type");
    TypeTemplateArgument Argument;
    return Argument;
}

TypeTemplateArgument TypeTemplateArgument::Clone() const
{
    TypeTemplateArgument ClonedArgument = *this;
    if ( ClonedArgument.Type == ETemplateArgumentType::TypeDeclaration && TypeConstant )
    {
        ClonedArgument.TypeConstant = TypeConstant->Clone();
    }
    if ( ClonedArgument.Type == ETemplateArgumentType::TypeMemberReference )
    {
        ClonedArgument.TypeMemberReference = TypeMemberReference.Clone();
    }
    return ClonedArgument;
}

size_t TypeTemplateArgument::GetTemplateArgumentHash() const
{
    switch (Type)
    {
        case ETemplateArgumentType::IntegerConst: return std::hash<int64_t>()( IntegerConstant );
        case ETemplateArgumentType::TypeDeclaration: return ITypeDeclaration::StaticGetDeclarationHash( TypeConstant );
        case ETemplateArgumentType::TypeMemberReference: return TypeMemberReference.GetTypeMemberReferenceHash();
        case ETemplateArgumentType::IntegerWildcard: return std::hash<int64_t>()( WildcardIndex );
        case ETemplateArgumentType::TypeMemberReferenceWildcard: return std::hash<int64_t>()( WildcardIndex );
        default: assert(!L"Unknown type template argument type"); return 0;
    }
}

bool TypeTemplateArgument::ParseTemplateArguments(const std::wstring& InText, std::vector<TypeTemplateArgument>& OutArguments, bool bAllowWildcardTypes)
{
    FTypeTextParseHelper ParseHelper(InText, bAllowWildcardTypes);
    return ParseHelper.ParseTemplateArgumentsInternal( OutArguments );
}

bool operator==(const TypeTemplateArgumentContainer& A, const TypeTemplateArgumentContainer& B)
{
    return A.Arguments.size() == B.Arguments.size() && A.Arguments == B.Arguments;
}

bool TypeTemplateArgumentContainer::Match(const TypeTemplateArgumentContainer& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    if ( Arguments.size() != InMatchAgainst.Arguments.size() )
    {
        return false;
    }
    for ( int32_t i = 0; i < Arguments.size(); i++ )
    {
        if ( !Arguments[i].Match( InMatchAgainst.Arguments[i], MatchContext ) )
        {
            return false;
        }
    }
    return true;
}

void TypeTemplateArgumentContainer::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    bool bIsFirstArgument = true;
    for ( const TypeTemplateArgument& Argument : Arguments )
    {
        if ( !bIsFirstArgument )
        {
            TextWriter.Append(L", ");
        }
        bIsFirstArgument = false;
        Argument.Print(TextWriter, Rules);
    }
}

TypeTemplateArgumentContainer TypeTemplateArgumentContainer::Clone() const
{
    TypeTemplateArgumentContainer Container;
    for ( const TypeTemplateArgument& Argument : Arguments )
    {
        Container.Arguments.push_back( Argument.Clone() );
    }
    return Container;
}

TypeTemplateArgumentContainer TypeTemplateArgumentContainer::Substitute(const TypeDeclarationMatchContext& MatchContext) const
{
    TypeTemplateArgumentContainer Container;
    for ( const TypeTemplateArgument& Argument : Arguments )
    {
        Container.Arguments.push_back( Argument.Substitute( MatchContext ) );
    }
    return Container;
}

size_t TypeTemplateArgumentContainer::GetContainerHash() const
{
    size_t ResultHash = std::hash<size_t>()(Arguments.size());
    for ( const TypeTemplateArgument& Argument : Arguments )
    {
        HashCombine(ResultHash, Argument.GetTemplateArgumentHash());
    }
    return ResultHash;
}

bool TypeTemplateArgumentContainer::ParseTemplateArguments(const std::wstring& InText, TypeTemplateArgumentContainer& ArgumentContainer, bool bAllowWildcardTypes)
{
    return TypeTemplateArgument::ParseTemplateArguments( InText, ArgumentContainer.Arguments, bAllowWildcardTypes );
}

void InternalTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    // TODO: Re-enable this assert once the higher level code in CodeGeneration can handle internal type replacement
    // assert(!L"Printing internal types is not supported. You need to substitute them for actual type definitions before printing.");
    TextWriter.Append(Identifier == EInternalIdentifier::Lambda ? L"<lambda>" :
        Identifier == EInternalIdentifier::AnonymousTag ? L"<anonymous-tag>" :
        Identifier == EInternalIdentifier::UnnamedTag ? L"<unnamed-tag>" :
        Identifier == EInternalIdentifier::UnnamedType ? L"<unnamed-type>" :
        Identifier == EInternalIdentifier::UnnamedEnum ? L"<unnamed-enum>" : L"<unknown>");
}

bool InternalTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    // Internal types only match themselves
    return this == InOtherDeclaration.get();
}

std::shared_ptr<ITypeDeclaration> InternalTypeDeclaration::Clone() const
{
    return std::make_shared<InternalTypeDeclaration>( *this );
}

size_t InternalTypeDeclaration::GetDeclarationHash() const
{
    size_t ResultHash = std::hash<ETypeDeclarationId>()( GetId() );
    HashCombine( ResultHash, Identifier );
    HashCombine( ResultHash, InternalTypeName );
    HashCombine( ResultHash, LambdaIndex );
    return ResultHash;
}

void WildcardTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    if ( bIsConst )
    {
        TextWriter.Append(L"const ");
    }
    TextWriter.AppendFormat(L"?%d", WildcardIndex);
}

bool WildcardTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    if (InOtherDeclaration->GetId() == ETypeDeclarationId::WildcardType)
    {
        const WildcardTypeDeclaration& Other = static_cast<WildcardTypeDeclaration&>(*InOtherDeclaration);
        return bIsConst == Other.bIsConst && WildcardIndex == Other.WildcardIndex;
    }
    return false;
}

std::shared_ptr<ITypeDeclaration> WildcardTypeDeclaration::Substitute(const TypeDeclarationMatchContext& MatchContext)
{
    return MatchContext.SubstituteWildcard( WildcardIndex, bIsConst );
}

std::shared_ptr<ITypeDeclaration> WildcardTypeDeclaration::Clone() const
{
    return std::make_shared<WildcardTypeDeclaration>( *this );
}

bool WildcardTypeDeclaration::Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    return MatchContext.MatchWildcard( WildcardIndex, bIsConst, InMatchAgainst );
}

size_t WildcardTypeDeclaration::GetDeclarationHash() const
{
    size_t ResultHash = std::hash<ETypeDeclarationId>()( GetId() );
    HashCombine( ResultHash, bIsConst );
    HashCombine( ResultHash, WildcardIndex );
    return ResultHash;
}

void UDTTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    // Emit const
    if ( bIsConst )
    {
        TextWriter.Append(L"const ");
    }

    // Emit UDT kind prefix if we know what type this is and we want CSU specifiers
    if ( UDTKind.has_value() && Rules.HasAnyFlags( TypeFormattingRules::EmitCSUSpecifier ) )
    {
        TextWriter.AppendFormat(L"%s ", CppUDTKindToString(UDTKind.value()));
    }

    if ( !Rules.HasAnyFlags( TypeFormattingRules::SkipOuterScope ) )
    {
        if ( OuterType )
        {
            // We never want CSU specifier for the parent type
            OuterType->Print( TextWriter, Rules.InheritableFlags().MaskFlagsNonInheritable( TypeFormattingRules::EmitCSUSpecifier ) );
            TextWriter.Append(L"::");
        }
        else if ( bIsGlobalNamespace )
        {
            TextWriter.Append(L"::");
        }
        // GetRelativeScope does not currently support outer type formatting, so do not use it if there is an outer type
        const std::wstring RelativeOuterScope = OuterType || bIsGlobalNamespace ? OuterScope : Rules.GetRelativeScope( OuterScope );
        if ( !RelativeOuterScope.empty() )
        {
            TextWriter.Append(RelativeOuterScope).Append(L"::");
        }
    }

    // Append type name
    TextWriter.Append(ClassName);

    // Append template instantiation arguments
    if ( !TemplateArguments.Arguments.empty() )
    {
        TextWriter.Append(L"<");
        TemplateArguments.Print( TextWriter, Rules );
        TextWriter.Append(L">");
    }
}

bool UDTTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    if (InOtherDeclaration->GetId() == ETypeDeclarationId::UDT)
    {
        const UDTTypeDeclaration& Other = static_cast<UDTTypeDeclaration&>(*InOtherDeclaration);
        return ClassName == Other.ClassName &&
            OuterScope == Other.OuterScope &&
            bIsConst == Other.bIsConst &&
            bIsGlobalNamespace == Other.bIsGlobalNamespace &&
            TemplateArguments == Other.TemplateArguments &&
            StaticIdentical( OuterType, Other.OuterType );
    }
    return false;
}

bool UDTTypeDeclaration::Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    if (InMatchAgainst->GetId() == ETypeDeclarationId::UDT)
    {
        const UDTTypeDeclaration& Other = static_cast<UDTTypeDeclaration&>(*InMatchAgainst);
        return ClassName == Other.ClassName &&
            OuterScope == Other.OuterScope &&
            bIsConst == Other.bIsConst &&
            bIsGlobalNamespace == Other.bIsGlobalNamespace &&
            TemplateArguments.Match( Other.TemplateArguments, MatchContext ) &&
            // Do not allow wildcard matching of outer types
            StaticIdentical( OuterType, Other.OuterType );
    }
    return false;
}

std::shared_ptr<ITypeDeclaration> UDTTypeDeclaration::Substitute(const TypeDeclarationMatchContext& MatchContext)
{
    std::shared_ptr<UDTTypeDeclaration> TypeDecl = std::make_shared<UDTTypeDeclaration>();
    TypeDecl->ClassName = ClassName;
    TypeDecl->OuterScope = OuterScope;
    TypeDecl->bIsConst = bIsConst;
    TypeDecl->bIsGlobalNamespace = bIsGlobalNamespace;
    TypeDecl->TemplateArguments = TemplateArguments.Substitute( MatchContext );
    TypeDecl->OuterType = OuterType;
    return TypeDecl;
}

size_t UDTTypeDeclaration::GetDeclarationHash() const
{
    size_t ResultHash = std::hash<ETypeDeclarationId>()( GetId() );
    HashCombine( ResultHash, ClassName );
    HashCombine( ResultHash, OuterScope );
    HashCombine( ResultHash, bIsConst );
    HashCombine( ResultHash, bIsGlobalNamespace );
    HashCombine( ResultHash, UDTKind );
    HashCombine( ResultHash, StaticGetDeclarationHash( OuterType ) );

    return ResultHash;
}

std::shared_ptr<ITypeDeclaration> UDTTypeDeclaration::Clone() const
{
    return std::make_shared<UDTTypeDeclaration>( *this );
}

void EnumTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    // Emit const
    if ( bIsConst )
    {
        TextWriter.Append(L"const ");
    }
    // Emit enum specifier if we are asked to
    if ( Rules.HasAnyFlags( TypeFormattingRules::EmitEnumSpecifier ) )
    {
        TextWriter.Append(bIsEnumClass ? L"enum class " : L"enum ");
    }

    if ( !Rules.HasAnyFlags( TypeFormattingRules::SkipOuterScope ) )
    {
        if ( OuterType )
        {
            // We never want CSU specifier for the parent type
            OuterType->Print( TextWriter, Rules.InheritableFlags().MaskFlagsNonInheritable( TypeFormattingRules::EmitCSUSpecifier ) );
            TextWriter.Append(L"::");
        }
        else if ( bIsGlobalNamespace )
        {
            TextWriter.Append(L"::");
        }
        // GetRelativeScope does not currently support outer type formatting, so do not use it if there is an outer type
        const std::wstring RelativeOuterScope = OuterType || bIsGlobalNamespace ? OuterScope : Rules.GetRelativeScope( OuterScope );
        if ( !RelativeOuterScope.empty() )
        {
            TextWriter.Append(RelativeOuterScope).Append(L"::");
        }
    }

    // Print enum name
    TextWriter.Append(EnumName);

    // Emit parent class if we have one and we are printing a specifier
    if ( UnderlyingType != nullptr && Rules.HasAnyFlags( TypeFormattingRules::EmitEnumSpecifier ) )
    {
        TextWriter.Append(L" : ");
        UnderlyingType->Print(TextWriter, Rules.InheritableFlags());
    }
}

bool EnumTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    if (InOtherDeclaration->GetId() == ETypeDeclarationId::Enum)
    {
        const EnumTypeDeclaration& Other = static_cast<EnumTypeDeclaration&>(*InOtherDeclaration);
        return bIsConst == Other.bIsConst &&
            bIsGlobalNamespace == Other.bIsGlobalNamespace &&
            bIsEnumClass == Other.bIsEnumClass &&
            EnumName == Other.EnumName &&
            OuterScope == Other.OuterScope &&
            StaticIdentical( UnderlyingType, Other.UnderlyingType ) &&
            StaticIdentical( OuterType, Other.OuterType );
    }
    return false;
}

std::shared_ptr<ITypeDeclaration> EnumTypeDeclaration::Substitute(const TypeDeclarationMatchContext& MatchContext)
{
    return std::make_shared<EnumTypeDeclaration>( *this );
}

size_t EnumTypeDeclaration::GetDeclarationHash() const
{
    size_t ResultHash = std::hash<ETypeDeclarationId>()( GetId() );
    HashCombine( ResultHash, EnumName );
    HashCombine( ResultHash, OuterScope );
    HashCombine( ResultHash, bIsConst );
    HashCombine( ResultHash, bIsGlobalNamespace );
    HashCombine( ResultHash, bIsEnumClass );
    HashCombine( ResultHash, StaticGetDeclarationHash( OuterType ) );
    HashCombine( ResultHash, StaticGetDeclarationHash ( UnderlyingType ) );

    return ResultHash;
}

std::shared_ptr<ITypeDeclaration> EnumTypeDeclaration::Clone() const
{
    return std::make_shared<EnumTypeDeclaration>( *this );
}

void PointerTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    // Print our pointee type first
    PointeeType->Print(TextWriter, Rules.InheritableFlags());

    // If there is an owner type, this is a member function pointer, which precedes the pointer token
    if ( OwnerType )
    {
        TextWriter.Append(L" ");
        OwnerType->Print(TextWriter, Rules);
        TextWriter.Append(L"::");
    }

    // Print pointer or reference
    TextWriter.Append(bIsReference ? L"&" : L"*");

    // Optionally append const modifier
    if ( bIsConst )
    {
        TextWriter.Append(L" const");
    }
}

bool PointerTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    if (InOtherDeclaration->GetId() == ETypeDeclarationId::PointerType)
    {
        const PointerTypeDeclaration& Other = static_cast<PointerTypeDeclaration&>(*InOtherDeclaration);
        return bIsConst == Other.bIsConst && bIsReference == Other.bIsReference &&
            StaticIdentical( PointeeType, Other.PointeeType ) &&
            StaticIdentical( OwnerType, Other.OwnerType );
    }
    return false;
}

bool PointerTypeDeclaration::Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    if (InMatchAgainst->GetId() == ETypeDeclarationId::PointerType)
    {
        const PointerTypeDeclaration& Other = static_cast<PointerTypeDeclaration&>(*InMatchAgainst);
        return bIsConst == Other.bIsConst &&
            bIsReference == Other.bIsReference &&
            StaticMatch( PointeeType, Other.PointeeType, MatchContext ) &&
            StaticMatch( OwnerType, Other.OwnerType, MatchContext );
    }
    return false;
}

std::shared_ptr<ITypeDeclaration> PointerTypeDeclaration::Substitute(const TypeDeclarationMatchContext& MatchContext)
{
    std::shared_ptr<PointerTypeDeclaration> PointerType = std::make_shared<PointerTypeDeclaration>( *this );
    PointerType->PointeeType = StaticSubstitute( PointeeType, MatchContext );
    PointerType->OwnerType = StaticSubstitute( OwnerType, MatchContext );
    return PointerType;
}

std::shared_ptr<ITypeDeclaration> PointerTypeDeclaration::Clone() const
{
    return std::make_shared<PointerTypeDeclaration>( *this );
}

size_t PointerTypeDeclaration::GetDeclarationHash() const
{
    size_t PointeeHash = PointeeType->GetDeclarationHash();
    HashCombine( PointeeHash, GetId() );
    HashCombine( PointeeHash, bIsConst );
    HashCombine( PointeeHash, bIsReference );
    HashCombine( PointeeHash, StaticGetDeclarationHash( OwnerType ) );
    return PointeeHash;
}

void ArrayTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    Print( TextWriter, Rules, std::optional<std::wstring>() );
}

void ArrayTypeDeclaration::PrintVariableType(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::wstring& VariableName) const
{
    Print( TextWriter, Rules, std::optional( VariableName ) );
}

void ArrayTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::optional<std::wstring>& VariableName) const
{
    // Print our element type first
    ElementType->Print(TextWriter, Rules.InheritableFlags());

    if ( VariableName.has_value() )
    {
        TextWriter.AppendFormat(L" %s", VariableName->c_str());
    }

    // Print array brackets and dimension if we have it
    TextWriter.Append(L"[");
    if ( ArrayDimension.has_value() )
    {
        TextWriter.AppendFormat(L"%d", ArrayDimension.value());
    }
    TextWriter.Append(L"]");
}

bool ArrayTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    if (InOtherDeclaration->GetId() == ETypeDeclarationId::ArrayType)
    {
        const ArrayTypeDeclaration& Other = static_cast<ArrayTypeDeclaration&>(*InOtherDeclaration);
        return ArrayDimension == Other.ArrayDimension && StaticIdentical(ElementType, Other.ElementType);
    }
    return false;
}

bool ArrayTypeDeclaration::Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    if (InMatchAgainst->GetId() == ETypeDeclarationId::ArrayType)
    {
        const ArrayTypeDeclaration& Other = static_cast<ArrayTypeDeclaration&>(*InMatchAgainst);
        return ArrayDimension == Other.ArrayDimension && StaticMatch(ElementType, Other.ElementType, MatchContext);
    }
    return false;
}

std::shared_ptr<ITypeDeclaration> ArrayTypeDeclaration::Substitute(const TypeDeclarationMatchContext& MatchContext)
{
    std::shared_ptr<ArrayTypeDeclaration> ArrayType = std::make_shared<ArrayTypeDeclaration>();
    ArrayType->ArrayDimension = ArrayDimension;
    ArrayType->ElementType = StaticSubstitute( ElementType, MatchContext );
    return ArrayType;
}

std::shared_ptr<ITypeDeclaration> ArrayTypeDeclaration::Clone() const
{
    return std::make_shared<ArrayTypeDeclaration>( *this );
}

size_t ArrayTypeDeclaration::GetDeclarationHash() const
{
    size_t ResultHash = std::hash<ETypeDeclarationId>()( GetId() );
    HashCombine( ResultHash, ElementType->GetDeclarationHash() );
    HashCombine( ResultHash, ArrayDimension );
    HashCombine( ResultHash, bIsConst );

    return ResultHash;
}

void FunctionTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    Print( TextWriter, Rules, std::optional<std::wstring>() );
}

void FunctionTypeDeclaration::PrintVariableType(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::wstring& VariableName) const
{
    Print( TextWriter, Rules, std::optional( VariableName ) );
}

void FunctionTypeDeclaration::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::optional<std::wstring>& VariableName) const
{
    ReturnType->Print(TextWriter, Rules.InheritableFlags());

    assert( (OwnerType == nullptr || bIsFunctionPointer) && L"Member function signatures cannot be bare- they need an outer type (e.g. a pointer)" );
    if ( bIsFunctionPointer )
    {
        TextWriter.Append(L"(");
        if ( OwnerType != nullptr )
        {
            OwnerType->Print(TextWriter, Rules.InheritableFlags());
            TextWriter.Append(L"::");
        }
        TextWriter.Append(L"*");

        // Print calling convention after the pointer (because of member function pointers) but before variable name
        if ( Rules.HasAnyFlags( TypeFormattingRules::EmitCallingConvention ) && CallingConvention.has_value() )
        {
            if ( CallingConvention == ECallingConvention::CDecl )
            {
                TextWriter.Append(L"__cdecl");
            }
            else if ( CallingConvention == ECallingConvention::StdCall )
            {
                TextWriter.Append(L"__stdcall");
            }
            else if ( CallingConvention == ECallingConvention::FastCall )
            {
                TextWriter.Append(L"__fastcall");
            }
            if ( VariableName.has_value() )
            {
                TextWriter.Append(L" ");
            }
        }

        if ( VariableName.has_value() )
        {
            TextWriter.Append(VariableName.value());
        }
        TextWriter.Append(L")");
    }

    TextWriter.Append(L"(");
    bool bIsFirstParameter = true;
    for ( const auto& [ParameterName, ParameterType] : Arguments )
    {
        if ( !bIsFirstParameter )
        {
            TextWriter.Append(L", ");
        }
        bIsFirstParameter = false;
        if ( !ParameterName.empty() )
        {
            assert( ParameterType->IsInlineDeclaration() && L"Function signature parameter types should be inline" );
            ParameterType->Print(TextWriter, Rules.InheritableFlags());
            TextWriter.AppendFormat(L" %s", ParameterName.c_str());
        }
        else
        {
            ParameterType->Print(TextWriter, Rules.InheritableFlags());
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
    if ( OwnerType != nullptr && bIsConstMemberFunction )
    {
        TextWriter.Append(L" const");
    }
}

bool FunctionTypeDeclaration::Identical(const std::shared_ptr<ITypeDeclaration>& InOtherDeclaration) const
{
    if (InOtherDeclaration->GetId() == ETypeDeclarationId::FunctionType)
    {
        const FunctionTypeDeclaration& Other = static_cast<FunctionTypeDeclaration&>(*InOtherDeclaration);

        if ( StaticIdentical( ReturnType, Other.ReturnType ) &&
            StaticIdentical( OwnerType, Other.OwnerType ) &&
            bIsFunctionPointer == Other.bIsFunctionPointer &&
            bIsConstMemberFunction == Other.bIsConstMemberFunction &&
            Arguments.size() == Other.Arguments.size() )
        {
            for ( int32_t i = 0; i < Arguments.size(); i++ )
            {
                if ( !StaticIdentical( Arguments[i].second, Other.Arguments[i].second ) )
                {
                    return false;
                }
            }
            return true;
        }
    }
    return false;
}

bool FunctionTypeDeclaration::Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    if (InMatchAgainst->GetId() == ETypeDeclarationId::FunctionType)
    {
        const FunctionTypeDeclaration& Other = static_cast<FunctionTypeDeclaration&>(*InMatchAgainst);

        if ( StaticMatch( ReturnType, Other.ReturnType, MatchContext ) &&
            StaticMatch( OwnerType, Other.OwnerType, MatchContext ) &&
            bIsFunctionPointer == Other.bIsFunctionPointer &&
            bIsConstMemberFunction == Other.bIsConstMemberFunction &&
            Arguments.size() == Other.Arguments.size() )
        {
            for ( int32_t i = 0; i < Arguments.size(); i++ )
            {
                if ( !StaticMatch( Arguments[i].second, Other.Arguments[i].second, MatchContext ) )
                {
                    return false;
                }
            }
            return true;
        }
    }
    return false;
}

std::shared_ptr<ITypeDeclaration> FunctionTypeDeclaration::Substitute(const TypeDeclarationMatchContext& MatchContext)
{
    std::shared_ptr<FunctionTypeDeclaration> FunctionType = std::make_shared<FunctionTypeDeclaration>();
    FunctionType->ReturnType = StaticSubstitute( ReturnType, MatchContext );
    FunctionType->OwnerType = StaticSubstitute( OwnerType, MatchContext );
    FunctionType->bIsFunctionPointer = bIsFunctionPointer;
    FunctionType->bIsConstMemberFunction = bIsConstMemberFunction;

    for ( const std::pair<std::wstring, std::shared_ptr<ITypeDeclaration>>& ArgumentType : Arguments )
    {
        FunctionType->Arguments.push_back({ ArgumentType.first, StaticSubstitute( ArgumentType.second, MatchContext ) });
    }
    return FunctionType;
}

std::shared_ptr<ITypeDeclaration> FunctionTypeDeclaration::Clone() const
{
    return std::make_shared<FunctionTypeDeclaration>( *this );
}

size_t FunctionTypeDeclaration::GetDeclarationHash() const
{
    size_t ResultHash = std::hash<ETypeDeclarationId>()( GetId() );
    HashCombine( ResultHash, ReturnType );
    HashCombine( ResultHash, StaticGetDeclarationHash( OwnerType ) );
    HashCombine( ResultHash, OwnerType );
    HashCombine( ResultHash, bIsFunctionPointer );
    HashCombine( ResultHash, bIsConstMemberFunction );

    for (const auto& ArgumentType : Arguments | std::views::values)
    {
        HashCombine( ResultHash, ArgumentType->GetDeclarationHash() );
    }
    return ResultHash;
}

bool ITypeDeclaration::StaticMatch(const std::shared_ptr<ITypeDeclaration>& Match, const std::shared_ptr<ITypeDeclaration>& MatchAgainst, TypeDeclarationMatchContext& MatchContext)
{
    return Match == MatchAgainst || ( Match != nullptr && MatchAgainst != nullptr && Match->Match( MatchAgainst, MatchContext ) );
}

std::shared_ptr<ITypeDeclaration> ITypeDeclaration::StaticSubstitute(const std::shared_ptr<ITypeDeclaration>& Original, const TypeDeclarationMatchContext& MatchContext)
{
    return Original ? Original->Substitute( MatchContext ) : nullptr;
}

TypeTextToken FTypeTextParseHelper::PeekNextToken() const
{
    TypeTextToken Token;
    PeekNextTokenInternal( Offset, Token );
    return Token;
}

TypeTextToken FTypeTextParseHelper::PeekNextNextToken() const
{
    TypeTextToken FirstToken;
    const int32_t SecondTokenStartOffset = PeekNextTokenInternal( Offset, FirstToken );
    TypeTextToken SecondToken;
    PeekNextTokenInternal( SecondTokenStartOffset, SecondToken );
    return SecondToken;
}

size_t ITypeDeclaration::StaticGetDeclarationHash(const std::shared_ptr<ITypeDeclaration>& Decl)
{
    return Decl ? Decl->GetDeclarationHash() : 0;
}

TypeTextToken FTypeTextParseHelper::ConsumeNextToken()
{
    TypeTextToken Token;
    Offset = PeekNextTokenInternal( Offset, Token );
    return Token;
}

std::shared_ptr<ITypeDeclaration> TypeDeclarationMatchContext::SubstituteWildcard(const int32_t InWildcardIndex, const bool  bIsWildcardConst) const
{
    const std::shared_ptr<ITypeDeclaration> OriginalType = WildcardMatches.find( InWildcardIndex )->second;

    // Resulting type is additionally const if wildcard states it is. Const + nonconst = Const, Const + Const = Const, Nonconst + nonconst = Nonconst
    const std::shared_ptr<ITypeDeclaration> SubstitutedType = OriginalType->Substitute( *this );
    SubstitutedType->bIsConst |= bIsWildcardConst;
    return SubstitutedType;
}

int64_t TypeDeclarationMatchContext::SubstituteIntegerWildcard(const int32_t InWildcardIndex) const
{
    return IntegerWildcardMatches.find( InWildcardIndex )->second;
}

TypeMemberReference TypeDeclarationMatchContext::SubstituteTypeMemberReferenceWildcard(int32_t InWildcardIndex) const
{
    return TypeMemberReferenceWildcardMatches.find( InWildcardIndex )->second;
}

bool operator==(const TypeMemberReference& A, const TypeMemberReference& B)
{
    return ITypeDeclaration::StaticIdentical(A.OwnerType, B.OwnerType) && A.MemberName == B.MemberName;
}

bool TypeMemberReference::Match(const TypeMemberReference& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    // Allow partial matching of owner types with wildcards, but member names have to fully match
    return ITypeDeclaration::StaticMatch(OwnerType, InMatchAgainst.OwnerType, MatchContext) && MemberName == InMatchAgainst.MemberName;
}

void TypeMemberReference::Print(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules) const
{
    if (OwnerType)
    {
        OwnerType->Print(TextWriter, Rules);
    }
    TextWriter.Append(L"::");
    TextWriter.Append(MemberName);
}

TypeMemberReference TypeMemberReference::Substitute(const TypeDeclarationMatchContext& MatchContext) const
{
    // Allow partial matching of owner types with wildcards, but member names have to fully match
    TypeMemberReference NewMemberReference;
    NewMemberReference.OwnerType = ITypeDeclaration::StaticSubstitute(OwnerType, MatchContext);
    NewMemberReference.MemberName = MemberName;

    return NewMemberReference;
}

TypeMemberReference TypeMemberReference::Clone() const
{
    TypeMemberReference NewMemberReference;
    NewMemberReference.OwnerType = OwnerType ? OwnerType->Clone() : nullptr;
    NewMemberReference.MemberName = MemberName;

    return NewMemberReference;
}

size_t TypeMemberReference::GetTypeMemberReferenceHash() const
{
    size_t MemberReferenceHash = ITypeDeclaration::StaticGetDeclarationHash(OwnerType);
    HashCombine(MemberReferenceHash, MemberName);
    return MemberReferenceHash;
}

bool ITypeDeclaration::StaticIdentical(const std::shared_ptr<ITypeDeclaration>& A, const std::shared_ptr<ITypeDeclaration>& B)
{
    return A == B || ( A != nullptr && B != nullptr && A->GetId() == B->GetId() && A->Identical(B) );
}

bool TypeDeclarationMatchContext::MatchWildcard(int32_t InWildcardIndex, bool bIsWildcardConst, const std::shared_ptr<ITypeDeclaration>& InMatchAgainst)
{
    // If this is a const wildcard matching a const type, we need to evaluate it against the non-const variant and use that type as a type to match cagainst
    std::shared_ptr<ITypeDeclaration> TypeToMatchAgainst = InMatchAgainst;
    if ( bIsWildcardConst )
    {
        // If this is a const wildcard and the type we are matching against is not const, we do not match
        if ( !TypeToMatchAgainst->bIsConst )
        {
            return false;
        }
        TypeToMatchAgainst = TypeToMatchAgainst->Clone();
        TypeToMatchAgainst->bIsConst = false;
    }
    // Check if we have already matched something
    if ( const auto Iterator = WildcardMatches.find(InWildcardIndex); Iterator != WildcardMatches.end() )
    {
        // We use Identical instead of Match to match wildcards since they do not support recursive wildcard matching
        return ITypeDeclaration::StaticIdentical( Iterator->second, TypeToMatchAgainst );
    }
    WildcardMatches.insert({ InWildcardIndex, TypeToMatchAgainst });
    return true;
}

bool TypeDeclarationMatchContext::MatchIntegerWildcard(int32_t InWildcardIndex, int64_t InIntegerConstant)
{
    if ( const auto Iterator = IntegerWildcardMatches.find(InWildcardIndex); Iterator != IntegerWildcardMatches.end() )
    {
        return InIntegerConstant == Iterator->second;
    }
    IntegerWildcardMatches.insert({ InWildcardIndex, InIntegerConstant });
    return true;
}

bool TypeDeclarationMatchContext::MatchTypeMemberReferenceWildcard(int32_t InWildcardIndex, const TypeMemberReference& InTypeMemberReference)
{
    if ( const auto Iterator = TypeMemberReferenceWildcardMatches.find(InWildcardIndex); Iterator != TypeMemberReferenceWildcardMatches.end() )
    {
        return InTypeMemberReference == Iterator->second;
    }
    TypeMemberReferenceWildcardMatches.insert({ InWildcardIndex, InTypeMemberReference });
    return true;
}

bool ITypeDeclaration::Match(const std::shared_ptr<ITypeDeclaration>& InMatchAgainst, TypeDeclarationMatchContext& MatchContext) const
{
    // By default, match matches the behavior of Identical
    return GetId() == InMatchAgainst->GetId() && Identical( InMatchAgainst );
}

std::shared_ptr<ITypeDeclaration> ITypeDeclaration::Substitute(const TypeDeclarationMatchContext& MatchContext)
{
    return shared_from_this();
}

void ITypeDeclaration::PrintVariableType(FormattedTextWriter& TextWriter, const TypeFormattingRules& Rules, const std::wstring& VariableName) const
{
    Print( TextWriter, Rules );
    TextWriter.AppendFormat(L" %s", VariableName.c_str());
}

std::shared_ptr<ITypeDeclaration> ITypeDeclaration::ParseTypeDeclaration(const std::wstring& InText, bool bAllowWildcardTypes)
{
    FTypeTextParseHelper ParseHelper( InText, bAllowWildcardTypes );
    return ParseHelper.ParseCompleteTypeDeclaration();
}

bool FTypeTextParseHelper::ParseTemplateArgumentsInternal( std::vector<TypeTemplateArgument>& OutTemplateArguments )
{
    bool bIsFirstArgument = true;
    while (true)
    {
        // Stop parsing template arguments if it's end of line or is a template bracket
        TypeTextToken CurrentToken = PeekNextToken();
        if ( CurrentToken.Type == ETypeTextToken::EndOfLine || CurrentToken.Type == ETypeTextToken::TemplateRBracket )
        {
            return true;
        }
        // We expect comma otherwise, unless it's first argument
        if ( CurrentToken.Type != ETypeTextToken::Comma && !bIsFirstArgument )
        {
            assert(!L"Expected comma, > or end of stream, got a other token");
            return false;
        }
        // Consume the comma if this is not the first argument
        if ( !bIsFirstArgument )
        {
            ConsumeNextToken();
        }
        bIsFirstArgument = false;

        // We expect to parse either a number constant, a pointer, or a type name here
        const TypeTextToken ArgumentFirstToken = PeekNextToken();
        if ( ArgumentFirstToken.Type == ETypeTextToken::Integer )
        {
            ConsumeNextToken();

            // Integer constant passed as a template argument
            TypeTemplateArgument TemplateArgument;
            TemplateArgument.Type = ETemplateArgumentType::IntegerConst;
            TemplateArgument.IntegerConstant = ArgumentFirstToken.IntegerValue;
            OutTemplateArguments.push_back(TemplateArgument);
        }
        // Check for wildcard integer constant, in case we are allowed to parse them.
        else if ( ArgumentFirstToken.Type == ETypeTextToken::IntegerWildcard && bAllowWildcards )
        {
            ConsumeNextToken();

            TypeTemplateArgument TemplateArgument;
            TemplateArgument.Type = ETemplateArgumentType::IntegerWildcard;
            TemplateArgument.WildcardIndex = ArgumentFirstToken.WildcardIndex;
            OutTemplateArguments.push_back(TemplateArgument);
        }
        // Check for type member reference wildcard, if we are allowed to parse wildcards
        else if ( ArgumentFirstToken.Type == ETypeTextToken::TypeMemberReferenceWildcard && bAllowWildcards )
        {
            ConsumeNextToken();

            TypeTemplateArgument TemplateArgument;
            TemplateArgument.Type = ETemplateArgumentType::TypeMemberReferenceWildcard;
            TemplateArgument.WildcardIndex = ArgumentFirstToken.WildcardIndex;
            OutTemplateArguments.push_back(TemplateArgument);
        }
        // Check for reference, that would parse as a function/data pointer
        else if ( ArgumentFirstToken.Type == ETypeTextToken::Reference )
        {
            ConsumeNextToken();

            const TypeMemberReference TypeMemberReference = ParseTypeMemberReference();

            TypeTemplateArgument TemplateArgument;
            TemplateArgument.Type = ETemplateArgumentType::TypeMemberReference;
            TemplateArgument.TypeMemberReference = TypeMemberReference;
            OutTemplateArguments.push_back(TemplateArgument);
        }
        // This must be a type declaration otherwise
        else
        {
            const std::shared_ptr<ITypeDeclaration> TypeDeclaration = ParseCompleteTypeDeclaration();
            if ( !TypeDeclaration )
            {
                assert(!L"Failed to parse template argument as integral or floating point constant, data or function pointer, or a type declaration");
                return false;
            }

            // Type passed as the template argument
            TypeTemplateArgument TemplateArgument;
            TemplateArgument.Type = ETemplateArgumentType::TypeDeclaration;
            TemplateArgument.TypeConstant = TypeDeclaration;
            OutTemplateArguments.push_back(TemplateArgument);
        }
    }
}

TypeMemberReference FTypeTextParseHelper::ParseTypeMemberReference()
{
    // A more narrow subset of C++ syntax is allowed here compared to ParseSimpleTypeDeclaration. For example,
    // no CSU specifiers are allowed, and no base types are allowed at any point. It must be a sequence of identifiers, potentially with template arguments
    TypeTextToken CurrentToken = PeekNextToken();

    std::shared_ptr<UDTTypeDeclaration> CurrentOuterType = nullptr;
    std::vector<std::wstring_view> NamespaceTypeNameAndMemberNameSegments;

    // Parses currently buffered segment tokens into the current outer type
    const auto ParseBufferedSegmentsIntoTypeDeclarations = [&](const TypeTemplateArgumentContainer& TemplateArguments, const int32_t EndOffset = 0)
    {
        assert(!NamespaceTypeNameAndMemberNameSegments.empty() && L"Expected identifier when parsing member/data pointer, got nothing");

        // This is the first type we are parsing, assume all parts before the last token are namespace, and last token is the type name
        if ( CurrentOuterType == nullptr )
        {
            CurrentOuterType = std::make_shared<UDTTypeDeclaration>();
            const std::span NamespaceSpan{NamespaceTypeNameAndMemberNameSegments.data(), NamespaceTypeNameAndMemberNameSegments.size() - EndOffset - 1};

            // Initialize first chunk as the namespace and last segment as the class name, and append arguments
            CurrentOuterType->OuterScope = JoinToString(NamespaceSpan, L"::");
            CurrentOuterType->ClassName = NamespaceTypeNameAndMemberNameSegments[NamespaceTypeNameAndMemberNameSegments.size() - EndOffset - 1];
            CurrentOuterType->TemplateArguments = TemplateArguments;
        }
        // This is a nested type. That implies that each segment represents a nested type, and never a namespace
        else
        {
            for ( int32_t i = 0; i < NamespaceTypeNameAndMemberNameSegments.size() - EndOffset; i++ )
            {
                const std::shared_ptr<UDTTypeDeclaration> NewNestedType = std::make_shared<UDTTypeDeclaration>();

                NewNestedType->OuterType = CurrentOuterType;
                NewNestedType->ClassName = NamespaceTypeNameAndMemberNameSegments[i];
                NewNestedType->TemplateArguments = TemplateArguments;
                CurrentOuterType = NewNestedType;
            }
        }
        NamespaceTypeNameAndMemberNameSegments.erase(NamespaceTypeNameAndMemberNameSegments.begin(), NamespaceTypeNameAndMemberNameSegments.end() - EndOffset);
    };

    // Consume identifiers until we run out of them
    while ( CurrentToken.Type == ETypeTextToken::Identifier )
    {
        // Consume the current identifier token
        NamespaceTypeNameAndMemberNameSegments.push_back(CurrentToken.Identifier);
        ConsumeNextToken();
        CurrentToken = PeekNextToken();

        // If we have parsed a template opening bracket, consume it, parse template arguments and consume closing bracket
        if ( CurrentToken.Type == ETypeTextToken::TemplateLBracket )
        {
            ConsumeNextToken();
            TypeTemplateArgumentContainer TemplateArguments;
            const bool bParsedTemplateArguments = ParseTemplateArgumentsInternal(TemplateArguments.Arguments);

            assert(bParsedTemplateArguments && L"Failed to parse template arguments when parsing member/data pointer");

            CurrentToken = PeekNextToken();
            assert(CurrentToken.Type == ETypeTextToken::TemplateRBracket && L"Expected > when parsing member/data pointer, got another token");
            ConsumeNextToken();

            // Form a type name from the already assembled tokens
            ParseBufferedSegmentsIntoTypeDeclarations(TemplateArguments, 0);
            CurrentToken = PeekNextToken();
        }

        // If this is not a scope delimiter, break out of the loop
        if ( CurrentToken.Type != ETypeTextToken::ScopeDelimiter )
        {
            break;
        }
        ConsumeNextToken();
        CurrentToken = PeekNextToken();
    }

    // If there are any additional tokens buffered, consume them to make a new type (but leave the last token)
    if ( NamespaceTypeNameAndMemberNameSegments.size() > 1 )
    {
        ParseBufferedSegmentsIntoTypeDeclarations(TypeTemplateArgumentContainer{}, 1);
    }

    // Make sure we have a valid outer type now and one token for the member name
    assert(CurrentOuterType != nullptr && L"Expected typename preceding member name when parsing member/data pointer");
    assert(NamespaceTypeNameAndMemberNameSegments.size() == 1 && L"Expected member name when parsing member/data pointer");
    const std::wstring_view MemberName = NamespaceTypeNameAndMemberNameSegments[0];

    TypeMemberReference ResultMemberReference;
    ResultMemberReference.OwnerType = CurrentOuterType;
    ResultMemberReference.MemberName = MemberName;

    return ResultMemberReference;
}

std::shared_ptr<ITypeDeclaration> FTypeTextParseHelper::ParseCompleteTypeDeclaration()
{
    // Parse basic type declaration
    const std::shared_ptr<ITypeDeclaration> SimpleTypeDeclaration = ParseSimpleTypeDeclaration();
    if ( !SimpleTypeDeclaration )
    {
        assert(!L"Failed to parse basic type declaration");
        return nullptr;
    }

    // Build complex type until we run out of type tokens we can parse
    std::shared_ptr<ITypeDeclaration> CurrentType = SimpleTypeDeclaration;
    while (true)
    {
        TypeTextToken NextTypeToken = PeekNextToken();

        // Parse pointer type
        if ( NextTypeToken.Type == ETypeTextToken::Reference || NextTypeToken.Type == ETypeTextToken::Pointer )
        {
            ConsumeNextToken();

            const std::shared_ptr<PointerTypeDeclaration> PointerType = std::make_shared<PointerTypeDeclaration>();
            PointerType->PointeeType = CurrentType;
            PointerType->bIsReference = NextTypeToken.Type == ETypeTextToken::Reference;

            // Parse optional const type modifier applied to the pointer
            NextTypeToken = PeekNextToken();
            if ( NextTypeToken.Type == ETypeTextToken::TypeModifier && NextTypeToken.TypeModifier == ETypeModifier::Const )
            {
                ConsumeNextToken();
                PointerType->bIsConst = true;
            }
            CurrentType = PointerType;
        }
        // Parse array type
        else if ( NextTypeToken.Type == ETypeTextToken::ArrayLBracket )
        {
            ConsumeNextToken();
            NextTypeToken = PeekNextToken();

            const std::shared_ptr<ArrayTypeDeclaration> ArrayType = std::make_shared<ArrayTypeDeclaration>();
            ArrayType->ElementType = CurrentType;

            // Potentially parse next token as number of elements in the array
            if ( NextTypeToken.Type == ETypeTextToken::Integer )
            {
                ConsumeNextToken();
                ArrayType->ArrayDimension = NextTypeToken.IntegerValue;
                NextTypeToken = PeekNextToken();
            }

            // Should always end with ArrayRBracket
            if ( NextTypeToken.Type != ETypeTextToken::ArrayRBracket )
            {
                assert(!L"Expected ] to close array type declaration, but got another token");
                return nullptr;
            }
            ConsumeNextToken();
            // Arrays are the final token in the typename, anything following the array type is next type or part of higher level structure
            return CurrentType;
        }
        // Parse function signature type, or dumb type ordering rules
        else if ( NextTypeToken.Type == ETypeTextToken::LBracket || NextTypeToken.Type == ETypeTextToken::CallingConvention )
        {
            const std::shared_ptr<ITypeDeclaration> FunctionTypeDeclaration = ParseFunctionPointerDeclaration( CurrentType );
            if ( FunctionTypeDeclaration == nullptr )
            {
                assert(!L"Failed to parse function pointer type declaration");
                return nullptr;
            }
            return FunctionTypeDeclaration;
        }
        // This could be a declaration of the pointer to member type, or a part that is unrelated to this type. We need to parse it to be able to tell for sure
        else if ( NextTypeToken.Type == ETypeTextToken::Identifier )
        {
            FTypeTextParseHelper ForkedTypeParser = *this;
            const std::shared_ptr<ITypeDeclaration> PotentialOuterTypeIdentifier = ForkedTypeParser.ParseSimpleTypeDeclaration();
            const TypeTextToken PotentialScopeDelimiterToken = ForkedTypeParser.ConsumeNextToken();
            const TypeTextToken PotentialPointerToken = ForkedTypeParser.ConsumeNextToken();

            if ( PotentialOuterTypeIdentifier && PotentialScopeDelimiterToken.Type == ETypeTextToken::ScopeDelimiter && PotentialPointerToken.Type == ETypeTextToken::Pointer )
            {
                // Consume all the tokens we digested
                ParseSimpleTypeDeclaration();
                ConsumeNextToken();
                ConsumeNextToken();

                // Construct and initialize the pointer to member type declaration
                const std::shared_ptr<PointerTypeDeclaration> MemberPointerTypeDeclaration = std::make_shared<PointerTypeDeclaration>();

                MemberPointerTypeDeclaration->PointeeType = CurrentType;
                MemberPointerTypeDeclaration->OwnerType = PotentialOuterTypeIdentifier;
                MemberPointerTypeDeclaration->bIsReference = false;

                // This could be a multi-layer pointer type, e.g. pointer to a pointer to a pointer to member, so we keep parsing from this point on
                // in case we encounter additional type declaration details later on
                CurrentType = MemberPointerTypeDeclaration;
            }
            else
            {
                // If this is not a pointer to member type, we are done parsing this type declaration
                return CurrentType;
            }
        }
        // Any other token we are not aware of, return the current type
        else
        {
            return CurrentType;
        }
    }
}

std::shared_ptr<ITypeDeclaration> FTypeTextParseHelper::ParseFunctionPointerDeclaration( const std::shared_ptr<ITypeDeclaration>& ReturnType)
{
    TypeTextToken NextTypeToken = PeekNextToken();

    std::shared_ptr<ITypeDeclaration> OuterTypeIdentifier;
    std::optional<ECallingConvention> CallingConvention;
    bool bIsFunctionTypeConst = false;
    bool bIsPotentiallyPointerOrReferenceToArray = true;
    bool bIsFunctionPointerType = false;

    // Attempt to parse a calling convention if it is the first token encountered after the return type
    if ( NextTypeToken.Type == ETypeTextToken::CallingConvention ) {
        ConsumeNextToken();
        CallingConvention = NextTypeToken.CallingConvention;
        NextTypeToken = PeekNextToken();
        // If we found a calling convention, this is not a reference to array
        bIsPotentiallyPointerOrReferenceToArray = false;
    }

    // We should always expect the first token to be an opening bracket, either for argument list or for member function declaration
    assert(NextTypeToken.Type == ETypeTextToken::LBracket && L"Expected ( following the function return type declaration, got a different token");
    ConsumeNextToken();
    NextTypeToken = PeekNextToken();

    // Attempt to parse a calling convention if it is the first token encountered after the argument list. In that case, this is definitely a function pointer declaration
    if ( NextTypeToken.Type == ETypeTextToken::CallingConvention && !CallingConvention.has_value() ) {
        ConsumeNextToken();
        CallingConvention = NextTypeToken.CallingConvention;
        NextTypeToken = PeekNextToken();
        // If we found a calling convention, this is not a reference to array
        bIsPotentiallyPointerOrReferenceToArray = false;
        bIsFunctionPointerType = true;
    }

    // If this is an identifier (or a global namespace scope delimiter), we are parsing a member function pointer type
    if ( NextTypeToken.Type == ETypeTextToken::Identifier || NextTypeToken.Type == ETypeTextToken::ScopeDelimiter ) {

        // This can end up being a type of the first argument if we are parsing the argument list and not the member function pointer declaration,
        // so attempt to parse the type separately first and only commit if we succeeded
        FTypeTextParseHelper ForkedTypeParser = *this;
        const std::shared_ptr<ITypeDeclaration> PotentialOuterTypeIdentifier = ForkedTypeParser.ParseSimpleTypeDeclaration();
        const TypeTextToken PotentialNextToken = ForkedTypeParser.PeekNextToken();

        // If this is an actual member function pointer declaration, next type token should always be a scope delimiter
        // delimiting the outer type from the function pointer declaration. If it is not, we are parsing an argument list instead, and should discard our partial parsing results
        if ( PotentialOuterTypeIdentifier && PotentialNextToken.Type == ETypeTextToken::ScopeDelimiter ) {

            // Consume all the tokens that we speculatively consumed before
            ParseSimpleTypeDeclaration();
            NextTypeToken = PotentialNextToken;
            OuterTypeIdentifier = PotentialOuterTypeIdentifier;

            // Next token should be a scope delimiter following the type and separating it from function pointer
            if (NextTypeToken.Type != ETypeTextToken::ScopeDelimiter) {
                assert(!L"Expected ::, got another token when parsing member function pointer class name");
                return nullptr;
            }
            ConsumeNextToken();
            NextTypeToken = PeekNextToken();
            // Member function pointer syntax definitely cannot be parsed as reference to array
            bIsPotentiallyPointerOrReferenceToArray = false;
        }
    }

    // Potentially parse calling convention in case we did not parse a outer type
    if (OuterTypeIdentifier == nullptr && NextTypeToken.Type == ETypeTextToken::CallingConvention && !CallingConvention.has_value()) {
        ConsumeNextToken();
        CallingConvention = NextTypeToken.CallingConvention;
        NextTypeToken = PeekNextToken();
        // If we found a calling convention, this is not an reference to array
        bIsPotentiallyPointerOrReferenceToArray = false;
    }

    // Potentially consume out-of-order const modifier that would be applied to the pointer
    if (NextTypeToken.Type == ETypeTextToken::TypeModifier && NextTypeToken.TypeModifier == ETypeModifier::Const) {
        ConsumeNextToken();
        bIsFunctionTypeConst = true;
        NextTypeToken = PeekNextToken();
    }

    // If the next token is not a pointer or reference, this is a function type declaration and not a function pointer declaration
    // In that case we do not need to parse second layer of the calling convention and const-ness of the function pointer, but can skip directly to the argument list
    // Note that this declaration syntax is only valid for global function prototypes, and as such cannot be used if we already parsed an outer type
    // We also have to parse the function pointer if we already parsed it being marked with const modifier
    // We also have to treat this as a member function pointer declaration if we encountered a calling convention declaration inside the argument list
    if ( NextTypeToken.Type == ETypeTextToken::Pointer || NextTypeToken.Type == ETypeTextToken::Reference || OuterTypeIdentifier != nullptr || bIsFunctionTypeConst || bIsFunctionPointerType )
    {
        // Next token should be a function pointer type (or reference)
        // Reference is only valid if this function declaration can be parsed as pointer to array
        if ( NextTypeToken.Type != ETypeTextToken::Pointer && !(NextTypeToken.Type == ETypeTextToken::Reference && bIsPotentiallyPointerOrReferenceToArray) )
        {
            assert(!L"Expected pointer type in the function pointer declaration, got another token");
            return nullptr;
        }
        const bool bParsedReferenceInsteadOfPointer = NextTypeToken.Type == ETypeTextToken::Reference;
        ConsumeNextToken();
        NextTypeToken = PeekNextToken();
        bIsFunctionPointerType = true;

        // Potentially consume calling convention if we have not done so before
        // We cannot parse calling convention if we parsed reference instead of a pointer
        if ( NextTypeToken.Type == ETypeTextToken::CallingConvention && !CallingConvention.has_value() && !bParsedReferenceInsteadOfPointer )
        {
            ConsumeNextToken();
            CallingConvention = NextTypeToken.CallingConvention;
            NextTypeToken = PeekNextToken();
            // If we found a calling convention, this is not a reference to array
            bIsPotentiallyPointerOrReferenceToArray = false;
        }

        // And also potentially consume const modifier
        if ( NextTypeToken.Type == ETypeTextToken::TypeModifier && NextTypeToken.TypeModifier == ETypeModifier::Const && !bIsFunctionTypeConst )
        {
            ConsumeNextToken();
            bIsFunctionTypeConst = true;
            NextTypeToken = PeekNextToken();
        }

        // Next token should be a closing bracket for pointer declaration
        if ( NextTypeToken.Type != ETypeTextToken::RBracket )
        {
            assert(!L"Expected ) closing the function pointer declaration, got another token");
            return nullptr;
        }
        ConsumeNextToken();
        NextTypeToken = PeekNextToken();

        // Check if this is actually a pointer/reference to array declaration. In that case, next symbol would be an array left bracket, followed by optional dimension and closing bracket
        // After that, we consider the type declaration complete, and nothing past it is parsed
        if ( NextTypeToken.Type == ETypeTextToken::ArrayLBracket && bIsPotentiallyPointerOrReferenceToArray )
        {
            ConsumeNextToken();
            NextTypeToken = PeekNextToken();

            // Potentially digest the static array dimensions
            std::optional<int32_t> StaticArrayDimension;
            if ( NextTypeToken.Type == ETypeTextToken::Integer )
            {
                ConsumeNextToken();
                StaticArrayDimension = NextTypeToken.IntegerValue;
                NextTypeToken = PeekNextToken();
            }

            // Last token we parse should be a closing bracket of the array
            if ( NextTypeToken.Type != ETypeTextToken::ArrayRBracket )
            {
                assert(!L"Expected ] following the array size declaration, got another token");
                return nullptr;
            }
            ConsumeNextToken();
            NextTypeToken = PeekNextToken();

            const std::shared_ptr<ArrayTypeDeclaration> ArrayType = std::make_shared<ArrayTypeDeclaration>();
            ArrayType->ElementType = ReturnType;
            ArrayType->ArrayDimension = StaticArrayDimension;

            const std::shared_ptr<PointerTypeDeclaration> PointerType = std::make_shared<PointerTypeDeclaration>();
            PointerType->PointeeType = ArrayType;
            PointerType->bIsConst = bIsFunctionTypeConst;
            PointerType->bIsReference = bParsedReferenceInsteadOfPointer;
            return PointerType;
        }

        // Next should be an opening bracket for the argument list
        if ( NextTypeToken.Type != ETypeTextToken::LBracket )
        {
            assert(!L"Expected ( preceding the function pointer argument list declaration, got another token");
            return nullptr;
        }

        ConsumeNextToken();
        NextTypeToken = PeekNextToken();

        // Make sure we did not parse a reference instead of a pointer, now that we know this is a function pointer declaration and not a reference to array
        if ( bParsedReferenceInsteadOfPointer )
        {
            assert(!L"Expected * when parsing function pointer declaration, but got & instead");
            return nullptr;
        }
    }

    // Parse function argument list
    std::vector<std::pair<std::wstring, std::shared_ptr<ITypeDeclaration>>> FunctionArguments;
    bool bIsVariadicArguments = false;
    while ( true )
    {
        // Check if this is a variadic arguments specifier before we digest it as a type
        if ( NextTypeToken.Type != ETypeTextToken::VariadicArguments )
        {
            const std::shared_ptr<ITypeDeclaration> ArgumentType = ParseCompleteTypeDeclaration();
            if ( ArgumentType == nullptr )
            {
                assert(!L"Failed to parse function pointer argument type declaration");
                return nullptr;
            }

            NextTypeToken = PeekNextToken();

            // Consume next token if it is a parameter name
            std::wstring ParameterName;
            if ( NextTypeToken.Type == ETypeTextToken::Identifier )
            {
                ConsumeNextToken();
                ParameterName = NextTypeToken.Identifier;
                NextTypeToken = PeekNextToken();
            }
            FunctionArguments.push_back({ ParameterName, ArgumentType });
        }
        // Digest variadic arguments specifier otherwise
        else
        {
            ConsumeNextToken();
            bIsVariadicArguments = true;
            NextTypeToken = PeekNextToken();
        }

        // Next token should be either a comma or a bracket
        if ( NextTypeToken.Type == ETypeTextToken::RBracket )
        {
            ConsumeNextToken();
            NextTypeToken = PeekNextToken();
            break;
        }
        // We should never have more arguments after variadic argument specifier, comma at this point is an error
        if ( NextTypeToken.Type != ETypeTextToken::Comma && !bIsVariadicArguments )
        {
            assert(!L"Expected , or ) after argument type and name declaration, got another token");
            return nullptr;
        }
        ConsumeNextToken();
        NextTypeToken = PeekNextToken();
    }

    // We might optionally parse const modifier right after if this is a member function
    bool bIsConstMemberFunction = false;
    if ( OuterTypeIdentifier != nullptr && NextTypeToken.Type == ETypeTextToken::TypeModifier && NextTypeToken.TypeModifier == ETypeModifier::Const )
    {
        ConsumeNextToken();
        NextTypeToken = PeekNextToken();
        bIsConstMemberFunction = true;
    }

    // Create the resulting type
    const std::shared_ptr<FunctionTypeDeclaration> FunctionType = std::make_shared<FunctionTypeDeclaration>();
    FunctionType->Arguments = FunctionArguments;
    FunctionType->ReturnType = ReturnType;
    FunctionType->bIsFunctionPointer = bIsFunctionPointerType;
    FunctionType->OwnerType = OuterTypeIdentifier;
    FunctionType->bIsConstMemberFunction = bIsConstMemberFunction;
    FunctionType->bIsVariadicArguments = bIsVariadicArguments;
    FunctionType->CallingConvention = CallingConvention;
    FunctionType->bIsConst = bIsFunctionTypeConst;
    return FunctionType;
}

static bool IsTokenValidTypenameStart( const TypeTextToken& Token )
{
    return Token.Type == ETypeTextToken::Identifier || Token.Type == ETypeTextToken::ScopeDelimiter;
}

std::shared_ptr<ITypeDeclaration> FTypeTextParseHelper::ParsePartialSimpleDeclarationPartial( const std::shared_ptr<ITypeDeclaration>& OuterType, std::vector<ETypeModifier> AppliedTypeModifiers, bool bHasEnumSpecifier, bool bIsEnumClass, const std::optional<CppUDTKind>& CSUSpecifier )
{
    std::optional<EBasicType> FundamentalType;
    std::wstring OuterScope;
    std::wstring TypeName;
    bool bIsGlobalNamespace{false};
    const TypeTextToken FirstTypeToken = PeekNextToken();
    std::optional<TypeTextToken> InternalTypeToken;
    std::optional<TypeTextToken> WildcardTypeToken;
    bool bIsVoidType = false;

    // Check if this is a fundamental type, or an user defined type
    if ( FirstTypeToken.Type == ETypeTextToken::FundamentalType )
    {
        ConsumeNextToken();
        FundamentalType = FirstTypeToken.FundamentalType;
        if ( CSUSpecifier.has_value() )
        {
            assert(!L"Encountered class/union/struct specifier with a fundamental type name. Expected end of stream");
            return nullptr;
        }
        if ( bHasEnumSpecifier )
        {
            assert(!L"Encountered enum specifier with a fundamental type name. Expected name of the enumeration");
            return nullptr;
        }
    }
    // Check if this is void type
    else if ( FirstTypeToken.Type == ETypeTextToken::Void )
    {
        ConsumeNextToken();
        bIsVoidType = true;
    }
    // If we have any modifiers except for the const applied, this must be a fundamental type. And if we failed to parse the next token as fundamental type, assume it is int
    else if ( !AppliedTypeModifiers.empty() && !(AppliedTypeModifiers.size() == 1 && AppliedTypeModifiers[0] == ETypeModifier::Const) )
    {
        FundamentalType = EBasicType::Int;
    }
    // Check if this is an internal type token. If it is, we need to produce an additional outer type, and then keep parsing from this place
    else if ( FirstTypeToken.Type == ETypeTextToken::InternalIdentifier )
    {
        ConsumeNextToken();
        InternalTypeToken = FirstTypeToken;
    }
    // Check if this is a wildcard token. We are only allowed to parse wildcards in specific cases
    else if ( FirstTypeToken.Type == ETypeTextToken::TypeWildcard && bAllowWildcards )
    {
        ConsumeNextToken();
        WildcardTypeToken = FirstTypeToken;
    }
    // Otherwise, parse type name and namespace, and if it's defined in global namespace
    else if ( !ParseScopeAndTypeName( OuterScope, TypeName, bIsGlobalNamespace ) )
    {
        assert(!L"Expected identifier for type name, got different token");
        return nullptr;
    }
    // Check if the next token is a scope separator followed by the internal type. In that case, it's an internal type inside the specific scope
    else if ( PeekNextToken().Type == ETypeTextToken::ScopeDelimiter && PeekNextNextToken().Type == ETypeTextToken::InternalIdentifier )
    {
        ConsumeNextToken();
        InternalTypeToken = ConsumeNextToken();
    }

    // Convert std::nullptr_t into FundamentalType nullptr because it is 3 tokens that cannot be easily digested in the tokenizer as one
    if ( OuterType == nullptr && (OuterScope.empty() || OuterScope == L"std") && TypeName == L"nullptr_t" )
    {
        FundamentalType = EBasicType::Nullptr;
        OuterScope.clear();
        TypeName.clear();
    }

    std::vector<TypeTemplateArgument> TemplateArguments;
    std::shared_ptr<ITypeDeclaration> UnderlyingEnumType;
    TypeTextToken TypePostfixToken = PeekNextToken();
    bool bHasTemplateArguments = false;

    // Check for underlying type specifier for enum
    if ( TypePostfixToken.Type == ETypeTextToken::BaseClassDelimiter )
    {
        if ( !bHasEnumSpecifier )
        {
            assert(!L"Expected enum underlying type delimiter only after enum specifier, but none was found");
            return nullptr;
        }
        ConsumeNextToken();

        // Parse underlying enumeration type
        UnderlyingEnumType = ParseSimpleTypeDeclaration();
        if ( !UnderlyingEnumType )
        {
            assert(!L"Failed to parse underlying type for the enumeration type declaration");
            return nullptr;
        }
        TypePostfixToken = PeekNextToken();
    }
    // Check for template instantiation
    else if ( TypePostfixToken.Type == ETypeTextToken::TemplateLBracket )
    {
        if ( bHasEnumSpecifier )
        {
            assert(!L"Encountered template instantiation after enum specifier. Enumerations cannot be templated");
            return nullptr;
        }
        if ( FundamentalType.has_value() || bIsVoidType )
        {
            assert(!L"Encountered template instantiation after a fundamental type. Fundamental types are not templates.");
            return nullptr;
        }
        if ( InternalTypeToken.has_value() )
        {
            assert(!L"Encountered template instantiation after an internal type. Internal types cannot be templated.");
            return nullptr;
        }
        ConsumeNextToken();

        // Mark the type as having template arguments, even if the actual list is empty
        bHasTemplateArguments = true;

        // Parse template instantiation arguments
        if ( !ParseTemplateArgumentsInternal( TemplateArguments ) )
        {
            assert(!L"Failed to parse template instantiation arguments");
            return nullptr;
        }

        // Make sure it is followed up by a template closing bracket
        const TypeTextToken ClosingBracket = ConsumeNextToken();
        if ( ClosingBracket.Type != ETypeTextToken::TemplateRBracket )
        {
            assert(!L"Expected a closing bracket after template instantiation, got different token");
            return nullptr;
        }
        TypePostfixToken = PeekNextToken();
    }

    // Check against invalid parsing logic combining global namespace and also an outer type
    if ( bIsGlobalNamespace && OuterType != nullptr )
    {
        assert(!L"Global namespace identifier and outer type cannot be both set at the same time");
        return nullptr;
    }

    // Check against pre-declaration of nested types. Nested types cannot be pre-declared
    if ( ( bHasEnumSpecifier || CSUSpecifier.has_value() ) && OuterType != nullptr )
    {
        assert(!L"Nested types or enumerations cannot be predeclared, and as such cannot have an CSU specifier or an Enum specifier");
        return nullptr;
    }

    // Check if this is a parent templated type for a nested type. ParseScopeAndTypeName will not digest templated type names as scope, so we need to check
    // if the postfix token is actually the scope delimiter, and then consume it
    // bHasTemplateArguments will ensure that this is a normal user defined type and that has template arguments
    // Similar situation happens when we parse an internal type
    if ( ( bHasTemplateArguments || InternalTypeToken.has_value() ) && TypePostfixToken.Type == ETypeTextToken::ScopeDelimiter && IsTokenValidTypenameStart( PeekNextNextToken() ) )
    {
        ConsumeNextToken();

        std::shared_ptr<ITypeDeclaration> ResultOuterType;

        // If we do not have an internal type token parsed, this is a user defined type. However, even if we have internal type parsed, it can still be prefixed with a type name,
        // which would always represent a parent UDT type that we should parse. In that case, we should never have template arguments though
        if ( !InternalTypeToken.has_value() || !TypeName.empty() )
        {
            // We do not have complete information about this type. For example, we do not know the CSU specifier for this type
            // But the information we have is enough to construct a nested type with optional CV modifiers and CSU/enum specifier
            const std::shared_ptr<UDTTypeDeclaration> OuterUDTType = std::make_shared<UDTTypeDeclaration>();
            OuterUDTType->bIsGlobalNamespace = bIsGlobalNamespace;
            OuterUDTType->OuterType = OuterType;
            OuterUDTType->OuterScope = OuterScope;
            OuterUDTType->ClassName = TypeName;
            OuterUDTType->TemplateArguments.Arguments = TemplateArguments;
            ResultOuterType = OuterUDTType;
        }

        // Parse internal type token if there is one
        if ( InternalTypeToken.has_value() )
        {
            assert(TemplateArguments.empty() && L"Internal type declarations cannot have any template arguments");

            const std::shared_ptr<InternalTypeDeclaration> OuterInternalType = std::make_shared<InternalTypeDeclaration>();
            OuterInternalType->OuterType = OuterType;
            OuterInternalType->Identifier = InternalTypeToken->InternalIdentifier;
            OuterInternalType->InternalTypeName = InternalTypeToken->UnnamedEnumOrTypeVariableName;
            OuterInternalType->LambdaIndex = InternalTypeToken->LambdaIndex;
            ResultOuterType = OuterInternalType;
        }

        // Pass applied modifiers and CSU/enum specifiers to the child class, since they are applied to it and not to the parent
        // We do not pass the specifiers down since nested types cannot be predeclared
        return ParsePartialSimpleDeclarationPartial( ResultOuterType, AppliedTypeModifiers, false, false, std::optional<CppUDTKind>() );
    }

    // Check for additional type modifiers
    if ( TypePostfixToken.Type == ETypeTextToken::TypeModifier )
    {
        ConsumeNextToken();
        AppliedTypeModifiers.push_back( TypePostfixToken.TypeModifier );

        TypePostfixToken = PeekNextToken();
        while ( TypePostfixToken.Type == ETypeTextToken::TypeModifier )
        {
            AppliedTypeModifiers.push_back( TypePostfixToken.TypeModifier );
            ConsumeNextToken();
            TypePostfixToken = PeekNextToken();
        }
    }

    // Sort the resulting type modifier collection for the predictable ordering and equality checking
    std::ranges::stable_sort(AppliedTypeModifiers);

    // Check for compatibility of the type modifiers with the type
    // TODO: Check for conflicting/invalid type modifiers as well
    if ( !FundamentalType.has_value() && !AppliedTypeModifiers.empty() && !(AppliedTypeModifiers.size() == 1 && AppliedTypeModifiers[0] == ETypeModifier::Const) )
    {
        assert(!L"Type Modifiers except for const can only be applied to fundamental types");
        return nullptr;
    }

    // Make sure that we do not have an outer type if we parsed this as a fundamental type
    if ( FundamentalType.has_value() && OuterType != nullptr )
    {
        assert(!L"Fundamental types cannot be nested inside of the user defined types");
        return nullptr;
    }
    if ( WildcardTypeToken.has_value() && ( !AppliedTypeModifiers.empty() && !(AppliedTypeModifiers.size() == 1 && AppliedTypeModifiers[0] == ETypeModifier::Const) || CSUSpecifier.has_value() || OuterType != nullptr ) )
    {
        assert(!L"Type wildcards cannot have type modifiers (except for const), CSU specifier or outer type declaration");
        return nullptr;
    }
    const bool bIsConst = std::ranges::find(AppliedTypeModifiers, ETypeModifier::Const) != AppliedTypeModifiers.end();

    // Construct the void type
    if ( bIsVoidType )
    {
        const std::shared_ptr<VoidTypeDeclaration> VoidTypeDecl = std::make_shared<VoidTypeDeclaration>();
        VoidTypeDecl->bIsConst = bIsConst;
        return VoidTypeDecl;
    }
    // Construct the fundamental type. Const is handled as a type modifier here and not as a separate thing
    if ( FundamentalType.has_value() )
    {
        const std::shared_ptr<FundamentalTypeDeclaration> FundamentalTypeDecl = std::make_shared<FundamentalTypeDeclaration>();
        FundamentalTypeDecl->BasicType = FundamentalType.value();
        FundamentalTypeDecl->bIsShort = std::ranges::find(AppliedTypeModifiers, ETypeModifier::ShortModifier ) != AppliedTypeModifiers.end();
        FundamentalTypeDecl->bIsUnsigned = std::ranges::find(AppliedTypeModifiers, ETypeModifier::UnsignedModifier ) != AppliedTypeModifiers.end();

        const int32_t LongModifierCount = std::ranges::count(AppliedTypeModifiers, ETypeModifier::LongModifier );
        FundamentalTypeDecl->bIsLong = LongModifierCount == 1;
        FundamentalTypeDecl->bIsLongLong = LongModifierCount == 2;
        FundamentalTypeDecl->bIsConst = bIsConst;
        return FundamentalTypeDecl;
    }

    // Construct internal type from the token if we have one.
    if ( InternalTypeToken.has_value() )
    {
        const std::shared_ptr<InternalTypeDeclaration> InternalTypeDecl = std::make_shared<InternalTypeDeclaration>();
        InternalTypeDecl->OuterType = OuterType;
        InternalTypeDecl->Identifier = InternalTypeToken->InternalIdentifier;
        InternalTypeDecl->InternalTypeName = InternalTypeToken->UnnamedEnumOrTypeVariableName;
        InternalTypeDecl->LambdaIndex = InternalTypeToken->LambdaIndex;
        InternalTypeDecl->bIsConst = bIsConst;
        return InternalTypeDecl;
    }

    // Construct wildcard type
    if ( WildcardTypeToken.has_value() )
    {
        const std::shared_ptr<WildcardTypeDeclaration> WildcardTypeDecl = std::make_shared<WildcardTypeDeclaration>();
        WildcardTypeDecl->WildcardIndex = WildcardTypeToken->WildcardIndex;
        WildcardTypeDecl->bIsConst = bIsConst;
        return WildcardTypeDecl;
    }

    // Construct the enumeration type if it was marked as enum
    if ( bHasEnumSpecifier )
    {
        const std::shared_ptr<EnumTypeDeclaration> EnumType = std::make_shared<EnumTypeDeclaration>();
        EnumType->bIsGlobalNamespace = bIsGlobalNamespace;
        EnumType->OuterType = OuterType;
        EnumType->OuterScope = OuterScope;
        EnumType->EnumName = TypeName;
        EnumType->bIsConst = bIsConst;
        EnumType->bIsEnumClass = bIsEnumClass;
        EnumType->UnderlyingType = UnderlyingEnumType;
        return EnumType;
    }

    // Construct UDT otherwise
    const std::shared_ptr<UDTTypeDeclaration> UDTType = std::make_shared<UDTTypeDeclaration>();
    UDTType->bIsGlobalNamespace = bIsGlobalNamespace;
    UDTType->OuterType = OuterType;
    UDTType->OuterScope = OuterScope;
    UDTType->ClassName = TypeName;
    UDTType->TemplateArguments.Arguments = TemplateArguments;
    UDTType->bIsConst = bIsConst;
    UDTType->UDTKind = CSUSpecifier;
    return UDTType;
}

// Simple type declaration is an enum, UDT, or a fundamental type declaration with optional CSU specifier and/or const modifier
std::shared_ptr<ITypeDeclaration> FTypeTextParseHelper::ParseSimpleTypeDeclaration()
{
    std::optional<CppUDTKind> CSUSpecifier;
    bool bHasEnumSpecifier = false;
    bool bIsEnumClass = false;
    std::vector<ETypeModifier> AppliedTypeModifiers;

    TypeTextToken TypePrefixToken = PeekNextToken();

    // Check for type modifiers preceding the type
    if ( TypePrefixToken.Type == ETypeTextToken::TypeModifier )
    {
        ConsumeNextToken();
        AppliedTypeModifiers.push_back( TypePrefixToken.TypeModifier );

        TypePrefixToken = PeekNextToken();
        while ( TypePrefixToken.Type == ETypeTextToken::TypeModifier )
        {
            AppliedTypeModifiers.push_back( TypePrefixToken.TypeModifier );
            ConsumeNextToken();
            TypePrefixToken = PeekNextToken();
        }
    }

    // Check for class/struct/union declaration
    if ( TypePrefixToken.Type == ETypeTextToken::UDTKindSpecifier )
    {
        ConsumeNextToken();
        CSUSpecifier = TypePrefixToken.UdtKindValue;
    }
    // Check for enum specifier and potentially enum class
    else if ( TypePrefixToken.Type == ETypeTextToken::EnumSpecifier )
    {
        ConsumeNextToken();
        bHasEnumSpecifier = true;

        // Check if this enum specifier is immediately followed by class specifier to create an enum class
        const TypeTextToken NextToken = PeekNextToken();
        if ( NextToken.Type == ETypeTextToken::UDTKindSpecifier && NextToken.UdtKindValue == CppUDTKind::Class )
        {
            ConsumeNextToken();
            bIsEnumClass = true;
        }
    }
    // Parse the rest of the type
    return ParsePartialSimpleDeclarationPartial( nullptr, AppliedTypeModifiers, bHasEnumSpecifier, bIsEnumClass, CSUSpecifier );
}

bool FTypeTextParseHelper::ParseScopeAndTypeName( std::wstring& OutScopeName, std::wstring& OutTypeName, bool& OutIsGlobalNamespace )
{
    bool bIsFirstTokenInStream = true;
    std::wstring PreviousTypeFragment;
    while (true)
    {
        const TypeTextToken NextToken = PeekNextToken();

        // Check if this is a scope delimiter. Scope delimiters should either start a type or follow an identifier
        // Only parse scope delimiter if the following token is an identifier. Otherwise, we might be parsing a type name in the member function declaration (something like void(Foo::Bar::*)())
        if ( NextToken.Type == ETypeTextToken::ScopeDelimiter && IsTokenValidTypenameStart( PeekNextNextToken() ) )
        {
            ConsumeNextToken();

            // Prevent two scope delimiters following each other being parsed as valid type name (e.g. ::::ClassName)
            if ( !bIsFirstTokenInStream && PreviousTypeFragment.empty() )
            {
                assert(!L"Invalid token preceding ::, expected a type name or stream start");
                return false;
            }
            // Identifiers starting with :: as first token represent names in global namespace and not the local namespace
            if ( bIsFirstTokenInStream )
            {
                OutIsGlobalNamespace = true;
            }
            // Otherwise, we expect another identifier following this scope delimiter, so the previous one is part of the scope
            else
            {
                if ( !OutScopeName.empty() )
                {
                    OutScopeName.append(L"::");
                }
                OutScopeName.append(PreviousTypeFragment);
                PreviousTypeFragment.clear();
            }
        }
        // Only parse an identifier if it is not preceded by another identifier (e.g. ClassNameA ClassNameB). In that case, ClassNameB is a part of something else
        else if ( NextToken.Type == ETypeTextToken::Identifier && PreviousTypeFragment.empty() )
        {
            ConsumeNextToken();
            PreviousTypeFragment = NextToken.Identifier;
        }
        // This is some kind of identifier that is not a part of a typename, so end our typename here
        else
        {
            // Make sure we parsed at least one type fragment
            if ( bIsFirstTokenInStream )
            {
                assert(!L"Expected identifier or ::, got stream end or different token");
                return false;
            }
            // We should always have the last name fragment here to use as a type name, if we do not last token we got was :: which is not correct
            if ( PreviousTypeFragment.empty() )
            {
                assert(!L"Type name ended with ::, should have ended with an identifier");
                return false;
            }
            OutTypeName = PreviousTypeFragment;
            return true;
        }
        bIsFirstTokenInStream = false;
    }
}

void FTypeTextParseHelper::SkipWhitespaces(int32_t& InOutOffset) const
{
    while ( InOutOffset < RawText.size() && iswspace(RawText[InOutOffset]) )
    {
        InOutOffset++;
    }
}

static bool IsValidTypeCharacter(const wchar_t InCharacter)
{
    return iswalnum(InCharacter) || InCharacter == L'_';
}

int32_t FTypeTextParseHelper::PeekNextTokenInternal(int32_t CurrentOffset, TypeTextToken& OutToken) const
{
    SkipWhitespaces( CurrentOffset );

    if ( CurrentOffset >= RawText.size() )
    {
        OutToken.Type = ETypeTextToken::EndOfLine;
        return CurrentOffset;
    }

    const wchar_t CurrentCharacter = RawText[CurrentOffset];
    if ( IsValidTypeCharacter(CurrentCharacter) && !iswdigit(CurrentCharacter) )
    {
        const int32_t StartOffset = CurrentOffset++;
        while ( CurrentOffset < RawText.size() && IsValidTypeCharacter(RawText[CurrentOffset]) )
        {
            CurrentOffset++;
        }
        const std::wstring_view IdentifierView( &RawText[StartOffset], CurrentOffset - StartOffset );

        // Check for reserved identifiers
        if ( IdentifierView == L"__cdecl" )
        {
            OutToken.Type = ETypeTextToken::CallingConvention;
            OutToken.CallingConvention = ECallingConvention::CDecl;
            return CurrentOffset;
        }
        if ( IdentifierView == L"__stdcall" )
        {
            OutToken.Type = ETypeTextToken::CallingConvention;
            OutToken.CallingConvention = ECallingConvention::StdCall;
            return CurrentOffset;
        }
        if ( IdentifierView == L"__fastcall" )
        {
            OutToken.Type = ETypeTextToken::CallingConvention;
            OutToken.CallingConvention = ECallingConvention::FastCall;
            return CurrentOffset;
        }
        if ( IdentifierView == L"void" )
        {
            OutToken.Type = ETypeTextToken::Void;
            return CurrentOffset;
        }
        if ( IdentifierView == L"const" )
        {
            OutToken.Type = ETypeTextToken::TypeModifier;
            OutToken.TypeModifier = ETypeModifier::Const;
            return CurrentOffset;
        }
        if ( IdentifierView == L"enum" )
        {
            OutToken.Type = ETypeTextToken::EnumSpecifier;
            return CurrentOffset;
        }
        if ( IdentifierView == L"struct" )
        {
            OutToken.Type = ETypeTextToken::UDTKindSpecifier;
            OutToken.UdtKindValue = CppUDTKind::Struct;
            return CurrentOffset;
        }
        if ( IdentifierView == L"class" )
        {
            OutToken.Type = ETypeTextToken::UDTKindSpecifier;
            OutToken.UdtKindValue = CppUDTKind::Class;
            return CurrentOffset;
        }
        if ( IdentifierView == L"union" )
        {
            OutToken.Type = ETypeTextToken::UDTKindSpecifier;
            OutToken.UdtKindValue = CppUDTKind::Union;
            return CurrentOffset;
        }
        if ( IdentifierView == L"signed" )
        {
            OutToken.Type = ETypeTextToken::TypeModifier;
            OutToken.TypeModifier = ETypeModifier::SignedModifier;
            return CurrentOffset;
        }
        if ( IdentifierView == L"unsigned" )
        {
            OutToken.Type = ETypeTextToken::TypeModifier;
            OutToken.TypeModifier = ETypeModifier::UnsignedModifier;
            return CurrentOffset;
        }
        if ( IdentifierView == L"short" )
        {
            OutToken.Type = ETypeTextToken::TypeModifier;
            OutToken.TypeModifier = ETypeModifier::ShortModifier;
            return CurrentOffset;
        }
        if ( IdentifierView == L"long" )
        {
            OutToken.Type = ETypeTextToken::TypeModifier;
            OutToken.TypeModifier = ETypeModifier::LongModifier;
            return CurrentOffset;
        }
        if ( IdentifierView == L"char" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Char;
            return CurrentOffset;
        }
        if ( IdentifierView == L"int" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Int;
            return CurrentOffset;
        }
        if ( IdentifierView == L"bool" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Bool;
            return CurrentOffset;
        }
        if ( IdentifierView == L"wchar_t" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::WideChar;
            return CurrentOffset;
        }
        if ( IdentifierView == L"char8" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Char8;
            return CurrentOffset;
        }
        if ( IdentifierView == L"char16" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Char16;
            return CurrentOffset;
        }
        if ( IdentifierView == L"char32" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Char32;
            return CurrentOffset;
        }
        if ( IdentifierView == L"float" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Float;
            return CurrentOffset;
        }
        if ( IdentifierView == L"double" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::Double;
            return CurrentOffset;
        }
        if ( IdentifierView == L"__int8" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::FixedLengthInt8;
            return CurrentOffset;
        }
        if ( IdentifierView == L"__int16" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::FixedLengthInt16;
            return CurrentOffset;
        }
        if ( IdentifierView == L"__int32" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::FixedLengthInt32;
            return CurrentOffset;
        }
        if ( IdentifierView == L"__int64" )
        {
            OutToken.Type = ETypeTextToken::FundamentalType;
            OutToken.FundamentalType = EBasicType::FixedLengthInt64;
            return CurrentOffset;
        }

        // Fallback to normal user defined identifier
        OutToken.Type = ETypeTextToken::Identifier;
        OutToken.Identifier = IdentifierView;
        return CurrentOffset;
    }
    if ( iswdigit(CurrentCharacter) || CurrentCharacter == '-' )
    {
        // Parse digits until we find non-digit token
        const int32_t StartOffset = CurrentOffset++;
        bool bFoundDecimalSeparator = false;
        while ( CurrentOffset < RawText.size() && iswdigit(RawText[CurrentOffset]) || (RawText[CurrentOffset] == L'.' && !bFoundDecimalSeparator) )
        {
            // Check if we just parsed a decimal separator. We should only have one
            if ( RawText[CurrentOffset] == L'.' )
            {
                bFoundDecimalSeparator = true;
            }
            CurrentOffset++;
        }
        const int32_t EndOffset = CurrentOffset;

        // Check if the current character is a floating point trailer, and ignore it if it is
        if ( CurrentOffset < RawText.size() && RawText[CurrentOffset] == L'f' )
        {
            CurrentOffset++;
            bFoundDecimalSeparator = true;
        }
        const std::wstring StringCopy( &RawText[StartOffset], EndOffset - StartOffset );

        // If we found a decimal separator or float trailer, parse as floating point constant. Otherwise, parse as integer constant
        if ( bFoundDecimalSeparator )
        {
            OutToken.Type = ETypeTextToken::Float;
            OutToken.FloatValue = _wtof( StringCopy.c_str() );
        }
        else
        {
            OutToken.Type = ETypeTextToken::Integer;
            OutToken.IntegerValue = _wtoll( StringCopy.c_str() );
        }
        return CurrentOffset;
    }
    // Match for type/integer wildcard. This is an invalid syntax in C++, even for compiler generated members
    if ( CurrentCharacter == L'?' && CurrentOffset + 1 < RawText.size() && ( iswdigit(RawText[CurrentOffset + 1]) || (
        (RawText[CurrentOffset + 1] == L'?' || RawText[CurrentOffset + 1] == L'&') && CurrentOffset + 2 < RawText.size() && iswdigit(RawText[CurrentOffset + 2]) ) ) )
    {
        // Parse digits until we find non-digit token
        const bool bIsLongWildcard = RawText[CurrentOffset + 1] == L'?' || RawText[CurrentOffset + 1] == L'&';
        CurrentOffset += bIsLongWildcard ? 2 : 1;
        const int32_t StartOffset = CurrentOffset++;
        while ( CurrentOffset < RawText.size() && iswdigit(RawText[CurrentOffset]) )
        {
            CurrentOffset++;
        }
        const int32_t EndOffset = CurrentOffset;
        const std::wstring StringCopy( &RawText[StartOffset], EndOffset - StartOffset );

        const bool bIsIntegerWildcard = bIsLongWildcard && RawText[CurrentOffset + 1] == L'?';
        const bool bIsTypeMemberReferenceWildcard = bIsLongWildcard && RawText[CurrentOffset + 1] == L'&';
        OutToken.Type = bIsIntegerWildcard ? ETypeTextToken::IntegerWildcard : (bIsTypeMemberReferenceWildcard ? ETypeTextToken::TypeMemberReferenceWildcard : ETypeTextToken::TypeWildcard);
        OutToken.WildcardIndex = _wtoi( StringCopy.c_str() );
        return CurrentOffset;
    }
    if ( CurrentCharacter == L'.' && CurrentOffset + 2 < RawText.size() && RawText[CurrentOffset + 1] == L'.' && RawText[CurrentOffset + 2] == L'.' )
    {
        OutToken.Type = ETypeTextToken::VariadicArguments;
        CurrentOffset += 3;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L':' && CurrentOffset + 1 < RawText.size() && RawText[CurrentOffset + 1] == L':' )
    {
        OutToken.Type = ETypeTextToken::ScopeDelimiter;
        CurrentOffset += 2;
        return CurrentOffset;
    }
    // This is an escaped identifier. We parse it until we encounter ', which is used as a closing delimiter for it
    // Escaped identifiers are only emitted by the compiler internals and are not a valid C++ syntax
    // NOTE: These can be nested apparently, at least on MSVC
    if ( CurrentCharacter == L'`' )
    {
        int32_t StartOffset = CurrentOffset;
        int32_t CurrentNestingDepth = 1;
        CurrentOffset++;
        while ( CurrentOffset < RawText.size() && CurrentNestingDepth > 0 )
        {
            // Handle nested internal identifiers
            if (RawText[CurrentOffset] == L'`') CurrentNestingDepth++;
            else if (RawText[CurrentOffset] == L'\'') CurrentNestingDepth--;
            CurrentOffset++;
        }
        assert(CurrentNestingDepth == 0 && L"Unbalanced escaped identifier parsed from token stream");

        OutToken.Type = ETypeTextToken::Identifier;
        OutToken.Identifier = std::wstring_view( &RawText[StartOffset + 1], CurrentOffset - StartOffset - 2 );
        return CurrentOffset;
    }
    if ( CurrentCharacter == L'*' )
    {
        OutToken.Type = ETypeTextToken::Pointer;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L'&' )
    {
        OutToken.Type = ETypeTextToken::Reference;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L'[' )
    {
        OutToken.Type = ETypeTextToken::ArrayLBracket;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L']' )
    {
        OutToken.Type = ETypeTextToken::ArrayRBracket;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L'(' )
    {
        OutToken.Type = ETypeTextToken::LBracket;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L')' )
    {
        OutToken.Type = ETypeTextToken::RBracket;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L'<' )
    {
        // Check for internal identifiers
        static const wchar_t* AnonymousTag = L"<anonymous-tag>";
        static const int32_t AnonymousTagLength = wcslen(AnonymousTag);
        static const wchar_t* UnnamedTag = L"<unnamed-tag>";
        static const int32_t UnnamedTagLength = wcslen(UnnamedTag);
        static const wchar_t* UnnamedTypePrefix = L"<unnamed-type-";
        static const int32_t UnnamedTypePrefixLength = wcslen(UnnamedTypePrefix);
        static const wchar_t* UnnamedEnumPrefix = L"<unnamed-enum-";
        static const int32_t UnnamedEnumPrefixLength = wcslen(UnnamedEnumPrefix);
        static const wchar_t* LambdaPrefix = L"<lambda_";
        static const int32_t LambdPrefixLength = wcslen(LambdaPrefix);

        // <anonymous-tag>
        if ( CurrentOffset + AnonymousTagLength < RawText.size() && std::wstring_view( &RawText[CurrentOffset], AnonymousTagLength ) == AnonymousTag )
        {
            OutToken.Type = ETypeTextToken::InternalIdentifier;
            OutToken.InternalIdentifier = EInternalIdentifier::AnonymousTag;
            CurrentOffset += AnonymousTagLength;
            return CurrentOffset;
        }
        // <unnamed-tag>
        if ( CurrentOffset + UnnamedTagLength < RawText.size() && std::wstring_view( &RawText[CurrentOffset], UnnamedTagLength ) == UnnamedTag )
        {
            OutToken.Type = ETypeTextToken::InternalIdentifier;
            OutToken.InternalIdentifier = EInternalIdentifier::UnnamedTag;
            CurrentOffset += UnnamedTagLength;
            return CurrentOffset;
        }
        // <unnamed-type-XXX>
        if ( CurrentOffset + UnnamedTypePrefixLength < RawText.size() && std::wstring_view( &RawText[CurrentOffset], UnnamedTypePrefixLength ) == UnnamedTypePrefix )
        {
            OutToken.Type = ETypeTextToken::InternalIdentifier;
            OutToken.InternalIdentifier = EInternalIdentifier::UnnamedType;
            CurrentOffset += UnnamedTypePrefixLength;

            int32_t UnnamedTypeNameStartOffset = CurrentOffset;
            while ( CurrentOffset < RawText.size() && RawText[CurrentOffset] != L'>' )
            {
                CurrentOffset++;
            }
            CurrentOffset++; // skip over the closing bracket
            OutToken.UnnamedEnumOrTypeVariableName = std::wstring_view( &RawText[UnnamedTypeNameStartOffset], CurrentOffset - UnnamedTypeNameStartOffset );
            return CurrentOffset;
        }
        // <unnamed-enum-XXX>
        if ( CurrentOffset + UnnamedEnumPrefixLength < RawText.size() && std::wstring_view( &RawText[CurrentOffset], UnnamedEnumPrefixLength ) == UnnamedEnumPrefix )
        {
            OutToken.Type = ETypeTextToken::InternalIdentifier;
            OutToken.InternalIdentifier = EInternalIdentifier::UnnamedEnum;
            CurrentOffset += UnnamedEnumPrefixLength;

            int32_t UnnamedTypeNameStartOffset = CurrentOffset;
            while ( CurrentOffset < RawText.size() && RawText[CurrentOffset] != L'>' )
            {
                CurrentOffset++;
            }
            CurrentOffset++; // skip over the closing bracket
            OutToken.UnnamedEnumOrTypeVariableName = std::wstring_view( &RawText[UnnamedTypeNameStartOffset], CurrentOffset - UnnamedTypeNameStartOffset );
            return CurrentOffset;
        }
        // <lambda_N>
        if ( CurrentOffset + LambdPrefixLength < RawText.size() && std::wstring_view( &RawText[CurrentOffset], LambdPrefixLength ) == LambdaPrefix )
        {
            OutToken.Type = ETypeTextToken::InternalIdentifier;
            OutToken.InternalIdentifier = EInternalIdentifier::UnnamedType;
            CurrentOffset += LambdPrefixLength;

            int32_t UnnamedTypeNameStartOffset = CurrentOffset;
            while ( CurrentOffset < RawText.size() && RawText[CurrentOffset] != L'>' )
            {
                CurrentOffset++;
            }
            CurrentOffset++; // skip over the closing bracket

            const std::wstring_view LambdaTextView( &RawText[UnnamedTypeNameStartOffset], CurrentOffset - UnnamedTypeNameStartOffset );
            OutToken.LambdaIndex = std::stoll( std::wstring( LambdaTextView ) );
            return CurrentOffset;
        }

        OutToken.Type = ETypeTextToken::TemplateLBracket;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L'>' )
    {
        OutToken.Type = ETypeTextToken::TemplateRBracket;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L',' )
    {
        OutToken.Type = ETypeTextToken::Comma;
        CurrentOffset++;
        return CurrentOffset;
    }
    if ( CurrentCharacter == L':' )
    {
        OutToken.Type = ETypeTextToken::BaseClassDelimiter;
        CurrentOffset++;
        return CurrentOffset;
    }
    CurrentOffset++;
    OutToken.Type = ETypeTextToken::Invalid;
    return CurrentOffset;
}
