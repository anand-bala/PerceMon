/// @file     parser/grammar.hpp
/// @brief    Definition of specification language grammar.

/// Conventions
/// ===========
///
/// - Use `using` to declare a rule that is a helper to build an actual rule in the
///   grammar.
/// - Use `struct` to define rules that affect the AST.

#pragma once

#ifndef PERCEMON_PARSER_GRAMMAR_HPP
#define PERCEMON_PARSER_GRAMMAR_HPP

#include <tao/pegtl.hpp>

namespace percemon::grammar {
namespace peg = tao::pegtl;

/// Line comments that starts using the character ';'
struct LineComment : peg::disable<peg::one<';'>, peg::until<peg::eolf>> {};

/// Any horizontal whitespace
struct Whitespace : peg::disable<peg::space> {};

/// Any horizontal white space or line comment
/// NOTE(anand): I am also going to include EOL as a a skippable comment.
struct Sep : peg::disable<peg::sor<Whitespace, LineComment, peg::eol>> {};
/// We use this as a placeholder for `Sep*`
struct Skip : peg::disable<peg::star<Sep>> {};

template <typename... R>
using rpad = peg::seq<R..., peg::at<Skip>>;

namespace sym {
using lparen    = rpad<peg::one<'('>>;
using rparen    = rpad<peg::one<')'>>;
using colon     = peg::one<':'>;
using semicolon = peg::one<';'>;

using double_quote = peg::one<'"'>;
using single_quote = peg::one<'\''>;
using vert_bar     = peg::one<'|'>;
using backslash    = peg::one<'\\'>;

using dot = peg::one<'.'>;

using plus          = peg::one<'+'>;
using minus         = peg::one<'-'>;
using plus_or_minus = peg::one<'+', '-'>;

using underscore = peg::one<'_'>;
using atsign     = peg::one<'@'>;
} // namespace sym

/// We use this to mark the end of a word/identifier (which is skippable whitespace)
struct EndOfWord : peg::not_at<peg::identifier_other> {};

template <typename K>
using key = peg::seq<K, EndOfWord>;

// General
// Primitives
struct KwTrue : key<TAO_PEGTL_STRING("true")> {};
struct KwFalse : key<TAO_PEGTL_STRING("false")> {};

using bool_primitives = peg::sor<KwTrue, KwFalse>;

// Propositional
struct KwNot : key<TAO_PEGTL_STRING("not")> {};
struct KwAnd : key<TAO_PEGTL_STRING("and")> {};
struct KwOr : key<TAO_PEGTL_STRING("or")> {};
struct KwImplies : key<TAO_PEGTL_STRING("implies")> {};
struct KwIff : key<TAO_PEGTL_STRING("iff")> {};
struct KwXor : key<TAO_PEGTL_STRING("xor")> {};

// Quantifiers
struct KwForall : key<TAO_PEGTL_STRING("forall")> {};
struct KwExists : key<TAO_PEGTL_STRING("exists")> {};

// Spatial Quantifiers (renamed from STQL)
struct KwSpatialExists : key<TAO_PEGTL_STRING("nonempty")> {};
struct KwSpatialForall : key<TAO_PEGTL_STRING("fills")> {};

// Other
struct KwPin : key<sym::atsign> {};

using builtin_ops =
    peg::sor<KwSpatialExists, KwSpatialForall, KwExists, KwForall, KwOr, KwAnd, KwNot>;

// Commands
struct KwSetOption : key<TAO_PEGTL_STRING("set-option")> {};
struct KwDefineFormula : key<TAO_PEGTL_STRING("define-formula")> {};
struct KwMonitor : key<TAO_PEGTL_STRING("monitor")> {};

using builtin_cmd = peg::sor<KwSetOption, KwDefineFormula, KwMonitor>;

// TODO(anand): Do built-in ops and types need to be reserved?
using reserved = peg::sor<builtin_cmd, builtin_ops, bool_primitives>;

// Built-in types
struct TypeBool : key<TAO_PEGTL_STRING("Bool")> {};
struct TypeInt : key<TAO_PEGTL_STRING("Int")> {};
struct TypeReal : key<TAO_PEGTL_STRING("Real")> {};

using builtin_type = peg::sor<TypeBool, TypeInt, TypeReal>;

/// Numeric Tokens and helpers
namespace num {
using bin_pre = TAO_PEGTL_STRING("#b");
using oct_pre = TAO_PEGTL_STRING("#o");
using hex_pre = TAO_PEGTL_STRING("#x");

using binary_digit  = peg::one<'0', '1'>;
using octal_digit   = peg::range<'0', '7'>;
using decimal_digit = peg::range<'0', '9'>;
using hex_digit     = peg::ranges<'0', '9', 'a', 'f', 'A', 'F'>;

using decimal_seq = peg::plus<decimal_digit>;
template <typename E>
using exponent =
    peg::if_must<E, peg::seq<peg::opt<sym::plus_or_minus>, peg::plus<peg::digit>>>;
} // namespace num

struct BinInt : peg::if_must<num::bin_pre, peg::plus<num::binary_digit>> {};
struct OctInt : peg::if_must<num::oct_pre, peg::plus<num::octal_digit>> {};
struct HexInt : peg::if_must<num::hex_pre, peg::plus<num::hex_digit>> {};
struct DecInt : peg::plus<peg::digit> {};

struct IntegerLiteral : peg::seq<peg::sor<BinInt, OctInt, HexInt, DecInt>> {};

struct DoubleLiteral : peg::seq<peg::sor<
                           peg::seq<
                               num::decimal_seq,
                               sym::dot,
                               num::decimal_seq,
                               peg::opt<num::exponent<peg::one<'e', 'E'>>>>,
                           peg::seq<
                               num::decimal_seq,
                               peg::opt<sym::dot>,
                               num::exponent<peg::one<'e', 'E'>>>>> {};

struct BooleanLiteral : peg::sor<KwTrue, KwFalse> {};

/// Character tokens and helpers
///
/// Since we are deriving this grammar from SMT-LIBv2, we don't really care about
/// escaping characters within strings, etc. Rather, we only escape the sequence "" to
/// be the character "
namespace chars {

/// All printable characters
using printable_char = peg::utf8::ranges<U'\u0020', U'\u007e', U'\u0080', U'\uffff'>;

/// List of escape sequences include `\b \t \n \f \r \" \\`.
using escaped =
    peg::if_must<sym::backslash, peg::one<'b', 't', 'n', 'f', 'r', '"', '\\'>>;
/// All printable characters excluding double quotes, and the escape
/// sequences `\b \t \n \f \r \" \\`.
using raw_string_char = peg::sor<escaped, printable_char>;

using symbol_special_chars = peg::one<
    '~',
    '!',
    '@',
    '$',
    '%',
    '^',
    '&',
    '*',
    '_',
    '-',
    '+',
    '=',
    '<',
    '>',
    '.',
    '?',
    '/'>;
/// A simple symbol can start with any ASCII letter and the characters `~ ! @ $ % ^
/// & * _ - + = < > . ? /`.
using simple_symbol_start = peg::sor<peg::alpha, symbol_special_chars>;
/// The tail of a simple symbol can contain any ASCII letter, digits, and the characters
/// `~ ! @ $ % ^ & * _ - + = < > . ? /`.
using simple_symbol_tail = peg::sor<peg::digit, peg::alpha, symbol_special_chars>;

/// All printable characters excluding backslash \ and vert |
using quoted_symbol_char = peg::sor<
    Whitespace,
    peg::utf8::ranges<
        U'\u0020',
        U'\u005b',
        U'\u005d',
        U'\u007b',
        U'\u007d',
        U'\u007e',
        U'\u0080',
        U'\uffff'>>;

} // namespace chars

/// A string literal is any sequence of characters between two double quotes, with the
/// literal escape sequence `\b \t \n \f \r \" \\`.
///
/// @note
/// This eschews from the definition of a raw string literal in SMT-LIB (even though
/// this specification language is inspired by it) because of the different use cases.
/// In SMT-LIB, they assume that strings can be inputs to the solver, thus, the
/// significance of the backslash and other "traditionally" escaped characters in C-like
/// languages is important, as these can change the input to the program.
///
/// \par
/// In PerceMon, strings have no significance other than to document various things. In
/// reality, this is here only for familiarity and may never be used by the script user.
struct StringLiteral : peg::if_must<
                           sym::double_quote,
                           peg::until<sym::double_quote, chars::raw_string_char>> {};

/// A simple symbol is a case sensitive sequence of characters that are either ASCII
/// letters, ASCII digits, or the characters `~ ! @ $ % ^ & * _ - + = < > . ? /`
struct SimpleSymbol : peg::seq<
                          peg::not_at<reserved>,
                          chars::simple_symbol_start,
                          peg::star<chars::simple_symbol_tail>> {};
/// A quoted symbol is any sequence of whitespace characters and printable characters
/// that starts and ends with `|` and does not contain `|`or `\`.
struct QuotedSymbol
    : peg::seq<peg::if_must<
          sym::vert_bar,
          peg::until<sym::vert_bar, peg::plus<chars::quoted_symbol_char>>>> {};

/// A valid symbol (mainly used as identifiers) is either a @ref QuotedSymbol or a @ref
/// SimpleSymbol.
struct Symbol : peg::sor<SimpleSymbol, QuotedSymbol> {};

/// A keyword is a token of the form `:<simple_symbol>`. They are used as attributes or
/// options for commands in PerceMon.
struct Keyword : peg::if_must<sym::colon, SimpleSymbol> {};

/* TODO(anand): Do we need indexed identifiers? */

struct SExpression;
/// A constant in the specification language can be a floating point constant, an
/// integer, a boolean literal, or a string.
struct Constant
    : peg::sor<DoubleLiteral, IntegerLiteral, BooleanLiteral, StringLiteral> {};

struct ParenExpr : peg::if_must<sym::lparen, SExpression, sym::rparen> {};
struct SExpression : peg::sor<Constant, Symbol, Keyword, ParenExpr> {};

/// Attributes are generally pairs consisting of an attribute name and an
/// associated value, although attributes with no value are also allowed.
struct OptionAttribute : Keyword {};
struct KeyValueAttribute : peg::seq<Keyword, Skip, SExpression> {};
struct Attribute : peg::sor<KeyValueAttribute, OptionAttribute> {};

template <typename... R>
using paren_surround = peg::if_must<sym::lparen, R..., sym::rparen>;

struct Term;

/// A qualified identifier is just a symbol. This can include one of the following
/// pre-defined identifiers/operators:
///
/// - `not`
/// - `and`
/// - `or`
/// - `implies`
/// - `iff`
/// - `xor`
/// - `always`
/// - `eventually`
/// - `until`
///
/// @note
/// We currently do not support annotated identifiers of the form `(as f Type)`
struct QualifiedIdentifier : Symbol {};
struct VarType : Symbol {};
struct VarDecl : paren_surround<Symbol, Skip, peg::opt<VarType>> {};
struct VarList : paren_surround<peg::list<Symbol, Skip>> {};

struct PinningExpression : peg::seq<KwPin, Skip, VarList> {};

using quantifier_ops = peg::sor<KwExists, KwForall>;
struct QuantifierExpression : peg::seq<quantifier_ops, Skip, VarList, Skip, Term> {};

struct Expression : peg::sor<
                        PinningExpression,
                        QuantifierExpression,
                        peg::seq<
                            QualifiedIdentifier,
                            Skip,
                            peg::list<Term, Skip>,
                            Skip,
                            peg::star<Attribute>>> {};
struct Term : peg::sor<paren_surround<Expression>, Constant, QualifiedIdentifier> {};

template <typename K, typename... S>
using cmd = peg::if_must<peg::seq<sym::lparen, K>, S..., sym::rparen>;

/// Set global options for the script.
struct CmdSetOption : peg::seq<KwSetOption, Skip, peg::star<Attribute, Skip>> {};

/// Define a formula.
///
/// @note
/// The formula must be strongly typed/well-sorted, i.e., all signals referred to in a
/// formula must be of the same type or types that are coercable to each other (e.g.
/// `Int` can be coerced to `Real`).
struct CmdDefineFormula : peg::seq<KwDefineFormula, Skip, Symbol, Skip, Term> {};

/// Define a monitor for a formula.
///
/// The monitor takes in a list of attributes that will set the semantics of the
/// monitor. The "name" of the identifier can be used to check the output of PerceMon.
struct CmdMonitor : peg::seq<KwMonitor, Skip, Symbol, Skip, peg::star<Attribute>> {};

using any_command = rpad<peg::if_must<
    sym::lparen,
    Skip,
    peg::sor<CmdSetOption, CmdDefineFormula, CmdMonitor>,
    Skip,
    sym::rparen>>;

struct StatementList : peg::until<peg::eof, peg::pad<any_command, Sep>> {};
/// A specification essentially consists for a list of top level commands,
/// andwe are just gonna ignore all horizontal spaces
struct Specification : peg::must<StatementList> {};

} // namespace percemon::grammar

#endif /* end of include guard: PERCEMON_PARSER_GRAMMAR_HPP */
