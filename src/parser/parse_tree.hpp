#pragma once

#ifndef PERCEMON_PARSER_PARSE_TREE_HPP
#define PERCEMON_PARSER_PARSE_TREE_HPP

#include <map>         // for map
#include <memory>      // for unique_ptr
#include <set>         // for set
#include <type_traits> // for true_type, false_type

#include "grammar.hpp"

namespace percemon {
struct Expr;
} // namespace percemon

namespace tao::pegtl::parse_tree {
struct node;
} // namespace tao::pegtl::parse_tree

namespace percemon::parser::parse_tree {

namespace gm = ::percemon::grammar;

template <typename Rule>
struct Selector : std::false_type {};

// clang-format off
template <> struct Selector<gm::BinInt>             : std::true_type {};
template <> struct Selector<gm::OctInt>             : std::true_type {};
template <> struct Selector<gm::HexInt>             : std::true_type {};
template <> struct Selector<gm::DecInt>             : std::true_type {};
template <> struct Selector<gm::IntegerLiteral>     : std::true_type {};
template <> struct Selector<gm::DoubleLiteral>      : std::true_type {};
template <> struct Selector<gm::BooleanLiteral>     : std::true_type {};
template <> struct Selector<gm::StringLiteral>      : std::true_type {};
template <> struct Selector<gm::SimpleSymbol>       : std::true_type {};
template <> struct Selector<gm::QuotedSymbol>       : std::true_type {};
template <> struct Selector<gm::Symbol>             : std::true_type {};
template <> struct Selector<gm::Keyword>            : std::true_type {};
template <> struct Selector<gm::Constant>           : std::true_type {};
template <> struct Selector<gm::SExpression>        : std::true_type {};
template <> struct Selector<gm::OptionAttribute>    : std::true_type {};
template <> struct Selector<gm::KeyValueAttribute>  : std::true_type {};
template <> struct Selector<gm::Attribute>          : std::true_type {};
template <> struct Selector<gm::QualifiedIdentifier>: std::true_type {};
template <> struct Selector<gm::VarType>            : std::true_type {};
template <> struct Selector<gm::VarDecl>            : std::true_type {};
template <> struct Selector<gm::Expression>         : std::true_type {};
template <> struct Selector<gm::Term>               : std::true_type {};
template <> struct Selector<gm::CmdDefineFormula>   : std::true_type {};
template <> struct Selector<gm::CmdMonitor>         : std::true_type {};
template <> struct Selector<gm::StatementList>      : std::true_type {};
template <> struct Selector<gm::Specification>      : std::true_type {};
// clang-format on

struct ParseContext {
  /// Creates a look-up table that maks parsed string contents to Expressions. Allows
  /// for reusing previously parsed expressions.
  std::map<std::string, std::shared_ptr<Expr>> lut;

  std::set<std::string> symbols;
  std::map<std::string, std::shared_ptr<Expr>> variables;

  /// List of defined formulas, keyed by their corresponding identifiers.
  std::map<std::string, std::shared_ptr<Expr>> defined_formulas;
  /// List of settings for monitors, keyed by their corresponding identifiers.
  std::map<std::string, std::shared_ptr<Expr>> monitors;
};

std::unique_ptr<ParseContext> transform(std::unique_ptr<tao::pegtl::parse_tree::node>);

} // namespace percemon::parser::parse_tree

#endif /* end of include guard: PERCEMON_PARSER_PARSE_TREE_HPP */
