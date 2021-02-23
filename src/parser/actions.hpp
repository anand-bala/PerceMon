/// @file parser/actions.hpp
/// @brief Defined actions that convert a parse tree to a concrete specification.
///
/// Conventions
/// ===========
///
/// 1. Use `apply` as far in the leaf nodes as possible. Use `apply0` otherwise.
/// 2. Use `utils::assert_` when we are looking for logic errors. Use
///    `pegtl::parse_error` when we are looking for parse errors. Difference is, one is
///    a mistake by us (the developers) and the other is a mistake by the users.
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

/* helpers */
/// Move the contents of an optional, and invalidate the optional.
///
/// Doesn't check if the optional has contents.
template <typename T>
T remove_opt_quick(std::optional<T>& opt) {
  auto content = std::move(*opt);
  opt          = std::nullopt;
  return content;
}

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
  /// A list of Terms parsed in the local context. For example, for an N-ary
  /// operation like And and Or, we expect the list to have at least 2 valid
  /// `ast::Expr`. This is populated when a local context is popped off the
  /// stack of a Term within the current rule.
  std::vector<std::shared_ptr<Expr>> terms;

  /// A Symbol. Needs to be immediately consumed by a parent rule.
  std::optional<std::string> symbol;

  /// Holds the variable name. Used immediately in VarDecl.
  std::optional<std::string> var_name;
  /// Holds the variable type. This is used immediately in a VarDecl rule.
  std::optional<std::string> var_type;
  /// A list of Variables (Terms) declared in the local context.
  ///
  /// Created in VarName with type `VarType::Unknown`. The parent rule will determine
  /// the type and the scope of the variable.
  /// Used by Pinning and Quantifier, and will be consumed immediately by the parent
  /// rule.
  std::vector<std::shared_ptr<Expr>> variables;
  /// Holds an interval constraint.
  ///
  /// To be used immediately by a parent temporal operation.
  std::optional<std::shared_ptr<Expr>> interval;

  /// A list of identifiers as parsed by the rule.
  ///
  /// In most cases, this is immediately used by the parent rule (either in
  /// Term or in some Command) to either create new formulas/assertions or to
  /// map existing formulas/assertions to a valid `ast::Expr`.
  std::vector<std::string> identifiers;

  /// An operation string. Used immediately by the Expression term.
  std::optional<std::string> operation;

  /// Keyword (`:<keyword>`). Consumed immediately in Attributes.
  std::optional<std::string> keyword;

  // TODO: Make attributes a pair. Options will have the second part at a nullptr.
  /// A list of option attributes as parsed by the rule.
  std::set<std::string> option_attributes;

  /// A list of option attributes as parsed by the rule.
  std::vector<std::pair<std::string, std::string>> keyval_attributes;
};

/// This maintains the global list of formulas and assertions that have been
/// parsed within the specification.
struct GlobalContext {
  /// List of defined formulas, keyed by their corresponding identifiers.
  std::map<std::string, std::shared_ptr<Expr>> defined_formulas;
  /// List of settings for monitors, keyed by their corresponding identifiers.
  std::map<std::string, std::shared_ptr<Expr>> monitors;

  /// List of global options
  std::set<std::string> options;
  /// List of global settings
  std::vector<std::pair<std::string, std::string>> settings;
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
struct action<gm::KwCTime> {
  static void apply0(GlobalContext&, ParserState& state) {
    state.result = Expr::Constant(PERCEMON_AST_NS::C_TIME{});
  }
};

template <>
struct action<gm::KwCFrame> {
  static void apply0(GlobalContext&, ParserState& state) {
    state.result = Expr::Constant(PERCEMON_AST_NS::C_FRAME{});
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
    state.symbol = in.string();
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
    state.symbol = content;
  }
};

template <>
struct action<gm::Keyword> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.symbol.has_value(), "Expected at least 1 symbol to parse as a keyword");
    utils::assert_(!state.keyword.has_value(), "Keyword seems to already have a value");
    state.keyword = remove_opt_quick(state.symbol);
  }
};

template <>
struct action<gm::OptionAttribute> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.keyword.has_value(), "Expected a :<keyword> to parse as an option");
    state.option_attributes.insert(remove_opt_quick(state.keyword));
  }
};

// TODO
template <>
struct action<gm::KeyValueAttribute> : peg::nothing<gm::KeyValueAttribute> {};

template <>
struct action<gm::QualifiedIdentifier> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.symbol.has_value(),
        "Expected a symbol to parse as a qualified identifier");
    state.identifiers.push_back(remove_opt_quick(state.symbol));
  }
};

template <>
struct action<gm::VarName> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.symbol.has_value(), "Expected a symbol to parse as a Variable name");
    state.var_name = remove_opt_quick(state.symbol);
  }
};

template <>
struct action<gm::VarType> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.symbol.has_value(), "Expected a symbol to parse as a Variable type");
    state.var_type = remove_opt_quick(state.symbol);
  }
};

template <>
struct action<gm::VarDecl> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.var_name.has_value(), "Expected a variable name to have been parsed");
    auto var_name = remove_opt_quick(state.var_name);
    auto var_type = state.var_type.value_or("Unknown");
    auto variable = Expr::Variable(var_name, var_type);
    state.variables.push_back(std::move(variable));
  }
};

// TODO: check if correct
template <>
struct action<gm::VarList> : peg::nothing<gm::VarList> {};

// template <>
// struct action<gm::KwPin> {
//   static void apply0(GlobalContext&, ParserState& state) {
//     state.operation = "@";
//   }
// };

template <>
struct action<gm::PinningExpression> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    const auto n_vars = state.variables.size();
    if (state.variables.empty() || n_vars > 2) {
      throw peg::parse_error(
          fmt::format(
              "Pinning operator expects either 1 or 2 pinned variables, got {}",
              n_vars),
          in);
    }
    auto variables = std::move(state.variables);

    auto vars_iter              = variables.begin();
    ExprPtr time_var            = nullptr;
    ExprPtr frame_var           = nullptr;
    ast::VarType first_var_type = ast::VarType::Unknown;
    { // Get the first one.
      ExprPtr varptr = std::move(*vars_iter);
      if (!std::holds_alternative<PERCEMON_AST_NS::Variable>(*varptr)) {
        throw peg::parse_error(
            fmt::format("Pin operation given an argument that is not a Variable"), in);
      }
      auto& var      = std::get<PERCEMON_AST_NS::Variable>(*varptr);
      first_var_type = var.type;
      switch (var.type) {
        case ast::VarType::Frame:
          frame_var = std::move(varptr);
          break;
        case ast::VarType::Timepoint:
          time_var = std::move(varptr);
          break;
        case ast::VarType::Unknown: // Time before frame
          var.type       = ast::VarType::Timepoint;
          time_var       = std::move(varptr);
          first_var_type = ast::VarType::Timepoint;
          break;
        default:
          throw peg::parse_error("Unsupported variable type", in);
      }
    }
    vars_iter = std::next(vars_iter);
    if (vars_iter != state.variables.end()) {
      // We already got one. Need to get the next one.
      ExprPtr varptr = std::move(*vars_iter);
      if (!std::holds_alternative<PERCEMON_AST_NS::Variable>(*varptr)) {
        throw peg::parse_error(
            fmt::format("Pin operation given an argument that is not a Variable"), in);
      }
      auto& var = std::get<PERCEMON_AST_NS::Variable>(*varptr);
      if (var.type == first_var_type) {
        throw peg::parse_error("Defining two pinned variables of the same type", in);
      } else if (
          var.type == ast::VarType::Unknown && first_var_type == ast::VarType::Frame) {
        throw peg::parse_error(
            "Order matters: Second unknown is assumed to be Frame variable, but looks like Frame variable was already defined",
            in);
      }
      switch (var.type) {
        case ast::VarType::Frame:
          frame_var = std::move(varptr);
          break;
        case ast::VarType::Timepoint:
          time_var = std::move(varptr);
          break;
        case ast::VarType::Unknown: // second is frame.
          var.type = ast::VarType::Frame;
          time_var = std::move(varptr);
          break;
        default:
          throw peg::parse_error("Unsupported variable type", in);
      }
    }

    // Create the result
    state.result = Expr::PinAt(time_var, frame_var);
  }
};

template <>
struct action<gm::IntervalExpression> {
  static void apply0(GlobalContext&, ParserState& state) {
    std::string operation = remove_opt_quick(state.operation);
    utils::assert_(operation == "_", "Expected an '_' operation to create intervals");
    utils::assert_(state.terms.size() == 1, "Expected exactly 1 Term as an operand");

    ExprPtr term = std::move(state.terms.back());
    state.terms.pop_back();

    state.interval = std::move(term);
  }
};

template <>
struct action<gm::quantifier_ops> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    auto content    = in.string();
    state.operation = std::move(content);
  }
};

template <>
struct action<gm::QuantifierExpression> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.operation.has_value(), "Expected a symbol for the quantifier operation");
    utils::assert_(
        state.variables.size() >= 1, "Expected at least 1 variable in the quantifier");
    utils::assert_(
        state.terms.size() == 1,
        "Expected exactly 1 sub-expression Term in the quantifier");
    std::string op_str = remove_opt_quick(state.operation);
    auto vars          = std::move(state.variables);
    auto terms         = std::move(state.terms);
    ExprPtr term       = std::move(terms.back());

    if (op_str == "exists") {
      state.result = Expr::Exists(vars, term);
    } else { //  Must be forall
      state.result = Expr::Forall(vars, term);
    }
  }
};

template <>
struct action<gm::Operation> {
  static void apply0(GlobalContext&, ParserState& state) {
    utils::assert_(
        state.symbol.has_value(), "Expected a symbol to parse as an operation");
    state.operation = remove_opt_quick(state.symbol);
  }
};

enum struct KnownOps {
  LT,
  LE,
  GT,
  GE,
  EQ,
  NEQ,
  NOT,
  AND,
  OR,
  IMPLIES,
  IFF,
  XOR,
  NEXT,
  PREV,
  EVENTUALLY,
  ONCE,
  ALWAYS,
  HISTORICALLY,
  UNTIL,
  SINCE,
  ADD,
  SUB,
  MUL,
  DIV,
  DIST,
  OFFSET,
  CLASS,
  PROB,
  AREA,
  BBOX,
  COMPLEMENT,
  INTERSECT,
  UNION,
  // INTERIOR,
  // CLOSURE,
  SPATIALEXISTS,
  SPATIALFORALL,
  SPATIALNEXT,
  SPATIALPREV,
  SPATIALEVENTUALLY,
  SPATIALONCE,
  SPATIALALWAYS,
  SPATIALHISTORICALLY,
  SPATIALUNTIL,
  SPATIALSINCE,
};
template <>
struct action<gm::OperationExpression> {
  static std::optional<KnownOps> lookup(std::string_view op) {
    // clang-format off
    if( op == "lt" || op == "<" ) {return KnownOps::LT;}
    if(op == "le" || op == "<=" ) {return KnownOps::LE;}
    if(op == "gt" || op == ">" ) {return KnownOps::GT;}
    if(op == "ge" || op == ">=" ) {return KnownOps::GE;}
    if(op == "eq" || op == "==" ) {return KnownOps::EQ;}
    if(op == "neq" || op == "!=" ) {return KnownOps::NEQ;}
    if(op == "not") {return KnownOps::NOT;}
    if(op == "and") {return KnownOps::AND;}
    if(op == "or") {return KnownOps::OR;}
    if(op == "next") {return KnownOps::NEXT;}
    if(op == "previous") {return KnownOps::PREV;}
    if(op == "eventually") {return KnownOps::EVENTUALLY;}
    if(op == "once") {return KnownOps::ONCE;}
    if(op == "always") {return KnownOps::ALWAYS;}
    if(op == "historically") {return KnownOps::HISTORICALLY;}
    if(op == "until") {return KnownOps::UNTIL;}
    if(op == "since") {return KnownOps::SINCE;}
    if(op == "add" || op == "+") {return KnownOps::ADD;}
    if(op == "sub" || op == "-") {return KnownOps::SUB;}
    if(op == "mul" || op == "*") {return KnownOps::MUL;}
    if(op == "div" || op == "/") {return KnownOps::DIV;}
    if(op == "dist" ) {return KnownOps::DIST;}
    if(op == "offset") {return KnownOps::OFFSET;}
    if(op == "class") {return KnownOps::CLASS;}
    if(op == "prob") {return KnownOps::PROB;}
    if(op == "area") {return KnownOps::AREA;}
    if(op == "bbox") {return KnownOps::BBOX;}
    if(op == "complement") {return KnownOps::COMPLEMENT;}
    if(op == "intersect") {return KnownOps::INTERSECT;}
    if(op == "union") {return KnownOps::UNION;}
    // if(op == "interior") {return KnownOps::INTERIOR;}
    // if(op == "closure") {return KnownOps::CLOSURE;}
    if(op == "spatial-exists") {return KnownOps::SPATIALEXISTS;}
    if(op == "spatial-forall") {return KnownOps::SPATIALFORALL;}
    if(op == "spatial-next") {return KnownOps::SPATIALNEXT;}
    if(op == "spatial-prev") {return KnownOps::SPATIALPREV;}
    if(op == "spatial-eventually") {return KnownOps::SPATIALEVENTUALLY;}
    if(op == "spatial-once") {return KnownOps::SPATIALONCE;}
    if(op == "spatial-always") {return KnownOps::SPATIALALWAYS;}
    if(op == "spatial-historically") {return KnownOps::SPATIALHISTORICALLY;}
    if(op == "spatial-until") {return KnownOps::SPATIALUNTIL;}
    if(op == "spatial-since") {return KnownOps::SPATIALSINCE;}
    // clang-format on

    return {};
  }

  template <typename ActionInput>
  static void handle_predicate(const ActionInput& in, ParserState& state, KnownOps op) {
    // 1. Check if we have 2 terms.
    if (state.terms.size() != 2) {
      throw peg::parse_error(
          fmt::format(
              "Predicate expects 2 arguments, an LHS and an RHS, got {}",
              state.terms.size()),
          in);
    }
    // 2. Load the args. Clear `terms`
    auto terms  = std::move(state.terms);
    ExprPtr lhs = std::move(terms[0]);
    ExprPtr rhs = std::move(terms[1]);
    // 3. Create result
    switch (op) {
      case KnownOps::LT:
        state.result = Expr::Lt(std::move(lhs), std::move(rhs));
        return;
      case KnownOps::LE:
        state.result = Expr::Le(std::move(lhs), std::move(rhs));
        return;
      case KnownOps::GT:
        state.result = Expr::Gt(std::move(lhs), std::move(rhs));
        return;
      case KnownOps::GE:
        state.result = Expr::Ge(std::move(lhs), std::move(rhs));
        return;
      case KnownOps::EQ:
        state.result = Expr::Eq(std::move(lhs), std::move(rhs));
        return;
      case KnownOps::NEQ:
        state.result = Expr::Neq(std::move(lhs), std::move(rhs));
        return;
      default:
        utils::unreachable();
    }
  }

  template <typename ActionInput>
  static void handle_unary(const ActionInput& in, ParserState& state, KnownOps op) {
    // 1. Check if we have 1 term.
    if (state.terms.size() != 1) {
      throw peg::parse_error(
          fmt::format("Unary operation expects 1 argument, got {}", state.terms.size()),
          in);
    }
    // 2. Load the arg. Clear `terms`.
    //    Load the options.
    auto terms       = std::move(state.terms);
    ExprPtr arg      = std::move(terms[0]);
    auto options     = std::move(state.option_attributes);
    auto options_set = std::set<std::string>{options.begin(), options.end()};
    // 3. Create result
    switch (op) {
      case KnownOps::NOT:
        state.result = Expr::Not(std::move(arg));
        return;
      case KnownOps::NEXT:
        state.result = Expr::Next(std::move(arg));
        return;
      case KnownOps::PREV:
        state.result = Expr::Previous(std::move(arg));
        return;
      case KnownOps::OFFSET:
        state.result = Expr::Offset(std::move(arg), std::move(options_set));
        return;
      case KnownOps::CLASS:
        state.result = Expr::Class(std::move(arg));
        return;
      case KnownOps::PROB:
        state.result = Expr::Prob(std::move(arg));
        return;
      case KnownOps::AREA:
        state.result = Expr::Area(std::move(arg));
        return;
      case KnownOps::BBOX:
        state.result = Expr::BBox(std::move(arg));
        return;
      case KnownOps::COMPLEMENT:
        state.result = Expr::Complement(std::move(arg));
        return;
      case KnownOps::SPATIALEXISTS:
        state.result = Expr::NotEmpty(std::move(arg));
        return;
      case KnownOps::SPATIALFORALL:
        state.result = Expr::Fills(std::move(arg));
        return;
      case KnownOps::SPATIALNEXT:
        state.result = Expr::SpatialNext(std::move(arg));
        return;
      case KnownOps::SPATIALPREV:
        state.result = Expr::SpatialPrevious(std::move(arg));
        return;
      default:
        utils::unreachable();
    }
  }

  template <typename ActionInput>
  static void handle_binary(const ActionInput& in, ParserState& state, KnownOps op) {
    // 1. Chck if we have exactly 2 terms
    if (state.terms.size() == 2) {
      throw peg::parse_error(
          fmt::format(
              "Binary operation expects exactly 2 arguments, got {}",
              state.terms.size()),
          in);
    }
    // 2. Load the args. (and the options)
    ExprPtr arg0     = std::move(state.terms[0]);
    ExprPtr arg1     = std::move(state.terms[1]);
    auto options     = std::move(state.option_attributes);
    auto options_set = std::set<std::string>{options.begin(), options.end()};

    // 3. Create the result
    switch (op) {
      case KnownOps::IMPLIES:
        state.result = Expr::Implies(arg0, arg1);
        return;
      case KnownOps::IFF:
        state.result = Expr::Iff(arg0, arg1);
        return;
      case KnownOps::XOR:
        state.result = Expr::Xor(arg0, arg1);
        return;
      case KnownOps::SUB:
        state.result = Expr::Subtract(arg0, arg1);
        return;
      case KnownOps::DIV:
        state.result = Expr::Div(arg0, arg1);
        return;
      case KnownOps::DIST:
        state.result = Expr::Dist(arg0, arg1, std::move(options_set));
        return;

      default:
        utils::unreachable();
    }
  }

  template <typename ActionInput>
  static void handle_nary(const ActionInput& in, ParserState& state, KnownOps op) {
    // 1. Check if we have at least 2 terms.
    if (state.terms.size() < 2) {
      throw peg::parse_error(
          fmt::format(
              "N-ary operation expects at least 2 arguments, got {}",
              state.terms.size()),
          in);
    }
    // 2. Load the args
    auto args = std::move(state.terms);
    // 3. Create result
    switch (op) {
      case KnownOps::AND:
        state.result = Expr::And(std::move(args));
        return;
      case KnownOps::OR:
        state.result = Expr::Or(std::move(args));
        return;
      case KnownOps::ADD:
        state.result = Expr::Add(std::move(args));
        return;
      case KnownOps::MUL:
        state.result = Expr::Mul(std::move(args));
        return;
      case KnownOps::UNION:
        state.result = Expr::Union(std::move(args));
        return;
      case KnownOps::INTERSECT:
        state.result = Expr::Intersect(std::move(args));
        return;
      default:
        utils::unreachable();
    }
  }

  template <typename ActionInput>
  static void
  handle_temporal_unary(const ActionInput& in, ParserState& state, KnownOps op) {
    // 1. Check if we have at least 2 terms.
    if (state.terms.size() == 1) {
      throw peg::parse_error(
          fmt::format(
              "Temporal Unary operation expects exactly 1 argument, got {}",
              state.terms.size()),
          in);
    }
    // 2. Load the args
    auto terms       = std::move(state.terms);
    ExprPtr arg      = std::move(terms[0]);
    ExprPtr interval = state.interval.value_or(nullptr);
    state.interval   = std::nullopt;

    // 3. Create result
    switch (op) {
      case KnownOps::EVENTUALLY:
        state.result = Expr::Eventually(std::move(arg), std::move(interval));
        return;
      case KnownOps::ONCE:
        state.result = Expr::Once(std::move(arg), std::move(interval));
        return;
      case KnownOps::ALWAYS:
        state.result = Expr::Always(std::move(arg), std::move(interval));
        return;
      case KnownOps::HISTORICALLY:
        state.result = Expr::Historically(std::move(arg), std::move(interval));
        return;
      case KnownOps::SPATIALEVENTUALLY:
        state.result = Expr::SpatialEventually(std::move(arg), std::move(interval));
        return;
      case KnownOps::SPATIALONCE:
        state.result = Expr::SpatialOnce(std::move(arg), std::move(interval));
        return;
      case KnownOps::SPATIALALWAYS:
        state.result = Expr::SpatialAlways(std::move(arg), std::move(interval));
        return;
      case KnownOps::SPATIALHISTORICALLY:
        state.result = Expr::SpatialHistorically(std::move(arg), std::move(interval));
        return;
      default:
        utils::unreachable();
    }
  }

  template <typename ActionInput>
  static void
  handle_temporal_binary(const ActionInput& in, ParserState& state, KnownOps op) {
    // 1. Check if we have 2 terms
    if (state.terms.size() == 2) {
      throw peg::parse_error(
          fmt::format(
              "Temporal Binary operation expects exactly 2 argument, got {}",
              state.terms.size()),
          in);
    }
    // 2. Load the args
    auto terms       = std::move(state.terms);
    ExprPtr arg0     = std::move(terms[0]);
    ExprPtr arg1     = std::move(terms[1]);
    ExprPtr interval = state.interval.value_or(nullptr);
    state.interval   = std::nullopt;

    // 3. Create result
    switch (op) {
      case KnownOps::UNTIL:
        state.result =
            Expr::Until(std::move(arg0), std::move(arg1), std::move(interval));
        return;
      case KnownOps::SINCE:
        state.result =
            Expr::Since(std::move(arg0), std::move(arg1), std::move(interval));
        return;
      case KnownOps::SPATIALUNTIL:
        state.result =
            Expr::SpatialUntil(std::move(arg0), std::move(arg1), std::move(interval));
        return;
      case KnownOps::SPATIALSINCE:
        state.result =
            Expr::SpatialSince(std::move(arg0), std::move(arg1), std::move(interval));
        return;
      default:
        utils::unreachable();
    }
  }

  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, ParserState& state) {
    utils::assert_(
        state.operation.has_value(), "Expected a symbol for the function operation");
    utils::assert_(state.terms.size() >= 1, "Expected at least 1 Term");

    std::string op_str         = remove_opt_quick(state.operation);
    std::optional<KnownOps> op = lookup(op_str);

    if (!op.has_value()) {
      auto options = std::set<std::string>{
          state.option_attributes.begin(), state.option_attributes.end()};
      state.result = Expr::Function(
          ast::FnType::Custom, std::move(state.terms), std::move(options));
      return;
    }

    switch (*op) {
      // Predicates
      case KnownOps::LT:
      case KnownOps::LE:
      case KnownOps::GT:
      case KnownOps::GE:
      case KnownOps::EQ:
      case KnownOps::NEQ:
        handle_predicate(in, state, *op);
        break;
      // Unary ops
      case KnownOps::NOT:
      case KnownOps::NEXT:
      case KnownOps::PREV:
      case KnownOps::OFFSET:
      case KnownOps::CLASS:
      case KnownOps::PROB:
      case KnownOps::AREA:
      case KnownOps::BBOX:
      case KnownOps::COMPLEMENT:
      case KnownOps::SPATIALEXISTS:
      case KnownOps::SPATIALFORALL:
      case KnownOps::SPATIALNEXT:
      case KnownOps::SPATIALPREV:
        handle_unary(in, state, *op);
        break;
      // Binary operations
      case KnownOps::IMPLIES:
      case KnownOps::IFF:
      case KnownOps::XOR:
      case KnownOps::SUB:
      case KnownOps::DIV:
      case KnownOps::DIST:
        handle_binary(in, state, *op);
        break;
      // Nary operations
      case KnownOps::AND:
      case KnownOps::OR:
      case KnownOps::ADD:
      case KnownOps::MUL:
      case KnownOps::UNION:
      case KnownOps::INTERSECT:
        handle_nary(in, state, *op);
        break;
      // Temporal unary operations.
      case KnownOps::EVENTUALLY:
      case KnownOps::ONCE:
      case KnownOps::ALWAYS:
      case KnownOps::HISTORICALLY:
      case KnownOps::SPATIALEVENTUALLY:
      case KnownOps::SPATIALONCE:
      case KnownOps::SPATIALALWAYS:
      case KnownOps::SPATIALHISTORICALLY:
        handle_temporal_unary(in, state, *op);
        break;
      // Temporal binary operations.
      case KnownOps::UNTIL:
      case KnownOps::SINCE:
      case KnownOps::SPATIALUNTIL:
      case KnownOps::SPATIALSINCE:
        handle_temporal_binary(in, state, *op);
    }

    // 4. Clear the attributes.
    state.option_attributes.clear();
    state.keyval_attributes.clear();
  }
};

template <>
struct action<gm::Term> {
  template <
      typename Rule,
      peg::apply_mode A,
      peg::rewind_mode M,
      template <typename...>
      class Action,
      template <typename...>
      class Control,
      typename ParseInput>
  static bool match(ParseInput& in, GlobalContext& global_state, ParserState& state) {
    // Here, we implement a push-down parser. Essentially, the new_state was
    // pushed onto the stack before the parser entered the rule for Term, and
    // now we have to pop the top of the stack (new_state) and merge the top
    // smartly with the old_state.

    // Create a new layer on the stack
    ParserState new_local_state{};
    new_local_state.level = state.level + 1;

    // Parse the input with the new state.
    bool ret = tao::pegtl::match<Rule, A, M, Action, Control>(
        in, global_state, new_local_state);
    // Once we are done parsing, we need to reset the states.
    // After the apply0 for Term was completed, new_state should have 1 Term in
    // the vector. This term needs to be moved onto the Terms vector in the
    // old_state.
    state.terms.insert(
        std::end(state.terms),
        std::begin(new_local_state.terms),
        std::end(new_local_state.terms));
    return ret;
  }

  template <typename ActionInput>
  static void
  apply(const ActionInput& in, GlobalContext& global_state, ParserState& state) {
    utils::assert_(
        state.terms.empty(),
        fmt::format("Expected 0 intermediary Terms, got {}", state.terms.size()));
    // Here, we have two possibilities:
    //
    // 1. The Term was a valid expression; or
    // 2. The Term was an identifier.
    //
    // In the first case, once we have a resultant Expression, we should move
    // it to a vector so that it can be used by parent nodes in the AST.
    //
    // In the second case, we have to copy the expression pointed by the
    // identifier onto the terms vector.

    // If we have an Expression as the sub-result.
    if (state.result) {
      // We move the result onto the vector of terms.
      state.terms.push_back(std::move(state.result));
    } else if (!state.identifiers.empty()) { // And if we have an id
      utils::assert_(
          state.identifiers.size() == 1,
          "Term should have matched exactly 1 identifier");
      // Copy the pointer to the formula with the corresponding id
      auto identifiers = std::move(state.identifiers);
      const auto id    = identifiers.back();
      const auto it    = global_state.defined_formulas.find(id);
      if (it == global_state.defined_formulas.end()) {
        throw peg::parse_error(
            fmt::format("Reference to unknown identifier: `{}`", id), in);
      } else {
        state.terms.push_back(it->second);
      }
    } else {
      // Otherwise, it doesn't make sense that there are no results, as this
      // means that the parser failed. This should be unreachable.
      //
      // UPDATE 2021-01-26:
      //
      // Turns out, this is reachable if there is an empty "()" in the list of
      // expressions. This means that we need to throw a parsing error, where we matched
      // an empty list where it never makes sense to have one.
      throw peg::parse_error(
          "Looks like a pair '(' ')' was matched with nothing in between", in);
      // utils::unreachable("Looks like a Term has no sub expression or identifier.");
    }
  }
};

template <>
struct action<gm::CmdSetOption> {
  static void apply0(GlobalContext& ctx, ParserState& state) {
    utils::assert_(!state.option_attributes.empty(), "Expected at least 1 option");
    ctx.options  = std::move(state.option_attributes);
    ctx.settings = std::move(state.keyval_attributes);
  }
};

template <>
struct action<gm::CmdDefineFormula> {
  static void apply0(GlobalContext& ctx, ParserState& state) {
    utils::assert_(
        state.identifiers.size() == 1, "Expected exactly 1 formula name identifier");
    utils::assert_(state.terms.size() == 1, "Expected exactly 1 expression argument");

    auto terms       = std::move(state.terms);
    auto identifiers = std::move(state.identifiers);

    auto expr = std::move(terms[0]);
    auto id   = std::move(identifiers[0]);

    ctx.defined_formulas[id] = std::move(expr);
  }
};

template <>
struct action<gm::valid_commands> {
  template <
      typename Rule,
      peg::apply_mode A,
      peg::rewind_mode M,
      template <typename...>
      class Action,
      template <typename...>
      class Control,
      typename ParseInput>
  static bool match(ParseInput& in, GlobalContext& global_state, ParserState&) {
    // We create a new local state for each top-level command. This way, we don't have
    // any pollution of scopes between commands (due to dev error).
    ParserState state{};
    state.level = 0;

    return tao::pegtl::match<Rule, A, M, Action, Control>(in, global_state, state);
  }

  static void apply0(GlobalContext&, ParserState&) {
    // Kept empty to satisfy PEGTL
  }
};

} // namespace percemon::parser::actions

#endif /* end of include guard: PERCEMON_PARSER_ACTIONS_HPP */
