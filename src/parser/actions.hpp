#pragma once

#ifndef PERCEMON_PARSER_ACTIONS_HPP
#define PERCEMON_PARSER_ACTIONS_HPP

#include "grammar.hpp"
#include "percemon/ast/expression.hpp"

#include "utils/static_analysis_helpers.hpp"

#include <fmt/core.h>
#include <tao/pegtl.hpp>

#include <cassert>
#include <map>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

namespace percemon::parser::actions {

/// Here, we will define the custom actions for the PEG parser that will
/// convert each rule into a valid AST class.
namespace peg = tao::pegtl;
namespace gm  = percemon::grammar;

/// This encodes local state of the push-down parser.
///
/// Essentially, this is a stack element, and a new one is pushed down
/// whenerver we encounter a Term, as that is the only recursive rule in our
/// grammar. This keeps track of whatever is required to make a Term, without
/// polluting the context of parent rules in the AST.
struct ParserState {
  /// Purely here for debugging purposes.
  unsigned long long int level;

  /// Whenever an Expression is completed, this field gets populared. Later,
  /// within the action for the Term rule, we move the result onto the vector
  /// `terms` to allow for the parent expression to easily combine it with
  /// their list of `terms`.
  std::unique_ptr<percemon::Expr> result;

  std::vector<std::string> symbols;
  std::vector<std::string> keywords;

  /// A list of identifiers as parsed by the rule.
  ///
  /// In most cases, this is immediately used by the parent rule (either in
  /// Term or in some Command) to either create new formulas/assertions or to
  /// map existing formulas/assertions to a valid `ast::Expr`.
  std::vector<std::string> identifiers;

  /// A list of option attributes as parsed by the rule.
  std::vector<std::string> option_attributes;

  /// A list of option attributes as parsed by the rule.
  std::vector<std::pair<std::string, percemon::ExprPtr>> keyval_attributes;

  /// A list of Terms parsed in the local context. For example, for an N-ary
  /// operation like And and Or, we expect the list to have at least 2 valid
  /// `ast::Expr`. This is populated when a local context is popped off the
  /// stack of a Term within the current rule.
  std::vector<std::shared_ptr<Expr>> terms;
};

/// This maintains the global list of formulas and assertions that have been
/// parsed within the specification.
struct GlobalContext {
  /// List of defined formulas, keyed by their corresponding identifiers.
  std::map<std::string, std::shared_ptr<Expr>> defined_formulas;
  /// List of settings for monitors, keyed by their corresponding identifiers.
  std::map<std::string, std::shared_ptr<Expr>> monitors;
};

template <typename Rule>
struct action : peg::nothing<Rule> {};

/// For each rule, we will assume that the top level function that calls this
/// action passes a reference to a `Specification` that we can populate, along
/// with other internal states.

template <>
struct action<gm::KwTrue> {
  static void apply0(GlobalContext&, ParserState& state) {
    state.result = Expr::Constant(true);
  }
};

template <>
struct action<gm::KwFalse> {
  static void apply0(GlobalContext&, ParserState& state) {
    state.result = Expr::Constant(false);
  }
};

template <>
struct action<gm::BinInt> {
  static constexpr int base = 2;

  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    long int val = std::stol(in.string(), 0, base);
    state.result = Expr::Constant(val);
  }
};

template <>
struct action<gm::OctInt> {
  static constexpr int base = 8;

  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    long int val = std::stol(in.string(), 0, base);
    state.result = Expr::Constant(val);
  }
};

template <>
struct action<gm::HexInt> {
  static constexpr int base = 16;

  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    long int val = std::stol(in.string(), 0, base);
    state.result = Expr::Constant(val);
  }
};

template <>
struct action<gm::DecInt> {
  static constexpr int base = 10;

  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    long int val = std::stol(in.string(), 0, base);
    state.result = Expr::Constant(val);
  }
};

template <>
struct action<gm::DoubleLiteral> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    double val   = std::stod(in.string());
    state.result = Expr::Constant(val);
  }
};

template <>
struct action<gm::StringLiteral> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    // Get the quoted content.
    std::string content = in.string();
    // Check if there is a " on either end and trim it.
    {
      size_t begin = 0, count = std::string::npos;
      if (content.front() == '"') {
        begin = 1;
      }
      if (content.back() == '"') {
        count = content.size() - begin - 1;
      }
      content = content.substr(begin, count);
    }
    state.result = Expr::Constant(content);
  }
};

template <>
struct action<gm::SimpleSymbol> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    state.symbols.push_back(in.string());
  }
};

template <>
struct action<gm::QuotedSymbol> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    std::string content = in.string();
    // We need to trim the '|' on either ends.
    {
      size_t begin = 0, count = std::string::npos;
      if (content.front() == '|') {
        begin = 1;
      }
      if (content.back() == '|') {
        count = content.size() - begin - 1;
      }
      content = content.substr(begin, count);
    }
    state.symbols.push_back(content);
  }
};

template <>
struct action<gm::Keyword> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    std::string content = in.string();
    // We need to trim the ':' at the front
    {
      size_t begin = 0, count = std::string::npos;
      if (content.front() == ':') {
        begin = 1;
      }
      content = content.substr(begin, count);
    }
    state.keywords.push_back(content);
  }
};

template <>
struct action<gm::OptionAttribute> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.keywords.size() >= 1,
        "Expected at least 1 :<keyword> to parse as an option");
    auto option = state.keywords.back();
    state.keywords.pop_back();
    state.option_attributes.push_back(option);
  }
};

// TODO
template <>
struct action<gm::KeyValueAttribute> : peg::nothing<gm::KeyValueAttribute> {};

} // namespace percemon::parser::actions

#endif /* end of include guard: PERCEMON_PARSER_ACTIONS_HPP */
