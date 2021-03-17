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
#include "utils/visit.hpp"

#include <fmt/core.h>
#include <magic_enum.hpp>
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
namespace peg   = tao::pegtl;
namespace gm    = percemon::grammar;
namespace nodes = percemon::ast::details;

using const_type = std::variant<bool, long long int, double, std::string>;

/// This encodes local state of the push-down parser.
///
/// Essentially, this is a stack element, and a new one is pushed down
/// whenerver we encounter a Term, as that is the only recursive rule in our
/// grammar. This keeps track of whatever is required to make a Term, without
/// polluting the context of parent rules in the AST.
struct Context {
  /// Purely here for debugging purposes.
  unsigned long long int level;

  /// List of parsed Constants
  ///
  /// Used immediately by either Term or AttributeValue
  std::vector<const_type> constants;

  /// A Symbol. Needs to be immediately consumed by a parent rule.
  std::optional<std::string> symbol;

  /// Keyword (`:<keyword>`). Consumed immediately in Attributes.
  std::optional<std::string> keyword;

  /// List of parsed attributes
  std::vector<ast::Attribute> attributes;

  /// An identifier as parsed by the rule.
  ///
  /// This is immediately used by the parent rule (either in Term or in some Command) to
  /// either create new formulas/assertions or to map existing formulas/assertions to a
  /// valid `ast::Expr`.
  std::optional<std::string> identifier;

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
  std::vector<ExprPtr> variables;

  /// An interval command. Error if there are more than 1 in an Term.
  std::shared_ptr<ast::details::Interval> interval;

  /// An operation string. Used immediately by the Expression term.
  std::optional<std::string> operation;

  /// Whenever an Expression is completed, this field gets populared. Later,
  /// within the action for the Term rule, we move the result onto the vector
  /// `terms` to allow for the parent expression to easily combine it with
  /// their list of `terms`.
  percemon::ExprPtr result;
  /// A list of Terms parsed in the local context. For example, for an N-ary
  /// operation like And and Or, we expect the list to have at least 2 valid
  /// `ast::Expr`. This is populated when a local context is popped off the
  /// stack of a Term within the current rule.
  std::vector<ExprPtr> terms;
};

/// This maintains the global list of formulas and assertions that have been
/// parsed within the specification.
struct GlobalContext {
  /// List of global settings
  AttrContainer settings;
  /// List of defined formulas, keyed by their corresponding identifiers.
  std::map<std::string, ExprPtr> defined_formulas;
  /// List of settings for monitors, keyed by their corresponding identifiers.
  std::map<std::string, ExprPtr> monitors;
};

template <typename Rule>
struct action : peg::nothing<Rule> {};

template <>
struct action<gm::KwTrue> {
  static void apply0(GlobalContext&, Context& ctx) {
    ctx.constants.emplace_back(true);
  }
};

template <>
struct action<gm::KwFalse> {
  static void apply0(GlobalContext&, Context& ctx) {
    ctx.constants.emplace_back(false);
  }
};

template <>
struct action<gm::KwCTime> {
  static void apply0(GlobalContext&, Context& state) {
    state.result = Expr::C_TIME();
  }
};

template <>
struct action<gm::KwCFrame> {
  static void apply0(GlobalContext&, Context& state) {
    state.result = Expr::C_FRAME();
  }
};

template <>
struct action<gm::BinInt> {
  static constexpr int base = 2;
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
    auto val = std::stoll(in.string(), /*pos*/ 0, base);
    ctx.constants.emplace_back(val);
  }
};

template <>
struct action<gm::OctInt> {
  static constexpr int base = 8;
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
    auto val = std::stoll(in.string(), /*pos*/ 0, base);
    ctx.constants.emplace_back(val);
  }
};

template <>
struct action<gm::HexInt> {
  static constexpr int base = 16;
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
    auto val = std::stoll(in.string(), /*pos*/ 0, base);
    ctx.constants.emplace_back(val);
  }
};

template <>
struct action<gm::DecInt> {
  static constexpr int base = 10;
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
    auto val = std::stoll(in.string(), /*pos*/ 0, base);
    ctx.constants.emplace_back(val);
  }
};

template <>
struct action<gm::DoubleLiteral> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
    auto val = std::stod(in.string());
    ctx.constants.emplace_back(val);
  }
};

template <>
struct action<gm::StringLiteral> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
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
    ctx.constants.emplace_back(content);
  }
};

template <>
struct action<gm::SimpleSymbol> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
    ctx.symbol = in.string();
  }
};

template <>
struct action<gm::QuotedSymbol> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
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
    ctx.symbol = content;
  }
};

template <>
struct action<gm::Keyword> {
  static void apply0(GlobalContext&, Context& state) {
    utils::assert_(
        state.symbol.has_value(), "Expected at least 1 symbol to parse as a keyword");
    utils::assert_(!state.keyword.has_value(), "Keyword seems to already have a value");
    state.keyword = remove_opt_quick(state.symbol);
  }
};

template <>
struct action<gm::Attribute> {
  static void apply0(GlobalContext&, Context& ctx) {
    utils::assert_(
        ctx.keyword.has_value(), "Expected a keyword to be parsed for attributes");
    std::string key = remove_opt_quick(ctx.keyword);
    auto vals       = std::move(ctx.constants);
    ctx.attributes.push_back(ast::Attribute{key, std::move(vals)});
  }
};

template <>
struct action<gm::QualifiedIdentifier> {
  static void apply0(GlobalContext&, Context& ctx) {
    utils::assert_(
        ctx.symbol.has_value(), "Expected a symbol to parse as a qualified identifier");
    utils::assert_(!ctx.identifier.has_value(), "Identifier already populated.");
    ctx.identifier = remove_opt_quick(ctx.symbol);
  }
};

template <>
struct action<gm::VarName> {
  static void apply0(GlobalContext&, Context& ctx) {
    utils::assert_(
        ctx.symbol.has_value(), "Expected a symbol to parse as a Variable name");
    ctx.var_name = remove_opt_quick(ctx.symbol);
  }
};

template <>
struct action<gm::VarType> {
  static void apply0(GlobalContext&, Context& ctx) {
    utils::assert_(
        ctx.symbol.has_value(), "Expected a symbol to parse as a Variable type");
    ctx.var_type = remove_opt_quick(ctx.symbol);
  }
};

template <>
struct action<gm::VarDecl> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& ctx) {
    utils::assert_(
        ctx.var_name.has_value(), "Expected a variable name to have been parsed");
    std::string var_name  = remove_opt_quick(ctx.var_name);
    ast::VarType var_type = ast::VarType::Unknown;
    if (ctx.var_type.has_value()) {
      std::string var_type_str = remove_opt_quick(ctx.var_type);
      auto var_type_p          = magic_enum::enum_cast<ast::VarType>(var_type_str);
      if (!var_type_p.has_value()) {
        throw peg::parse_error(
            fmt::format("Unknown Variable type: `{}`", var_type_str), in);
      } else {
        var_type = *var_type_p;
      }
    }
    auto variable = Expr::Variable(var_name, var_type);
    ctx.variables.push_back(std::move(variable));
  }
};

template <>
struct action<gm::PinningExpression> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& state) {
    const auto n_vars = state.variables.size();
    if (state.variables.empty() || n_vars > 2) {
      throw peg::parse_error(
          fmt::format(
              "Pinning operator expects either 1 or 2 pinned variables, got {}",
              n_vars),
          in);
    }
    if (state.terms.size() != 1) {
      throw peg::parse_error(
          fmt::format(
              "Pinning operator expects exactly 1 sub-expression, got {}",
              state.terms.size()),
          in);
    }
    auto variables = std::move(state.variables);

    auto vars_iter              = variables.begin();
    ExprPtr time_var            = nullptr;
    ExprPtr frame_var           = nullptr;
    ast::VarType first_var_type = ast::VarType::Unknown;
    { // Get the first one.
      ExprPtr varptr = std::move(*vars_iter);
      if (!std::holds_alternative<nodes::Variable>(*varptr)) {
        throw peg::parse_error(
            fmt::format("Pin operation given an argument that is not a Variable"), in);
      }
      auto& var      = std::get<nodes::Variable>(*varptr);
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
      if (!std::holds_alternative<nodes::Variable>(*varptr)) {
        throw peg::parse_error(
            fmt::format("Pin operation given an argument that is not a Variable"), in);
      }
      auto& var = std::get<nodes::Variable>(*varptr);
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

    auto terms = std::move(state.terms);

    // Create the result
    state.result = Expr::PinAt(time_var, frame_var, std::move(terms.back()));
  }
};

template <>
struct action<gm::interval_ops> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& state) {
    auto content    = in.string();
    state.operation = std::move(content);
  }
};

template <>
struct action<gm::IntervalExpression> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& state) {
    utils::assert_(
        state.operation.has_value(), "Expected an operation to have been parser");
    utils::assert_(state.terms.size() == 2, "Expected exactly 2 Term as an operand");

    std::string operation = remove_opt_quick(state.operation);
    auto terms            = std::move(state.terms);
    try {
      if (operation == "_frame") { // Frame Interval
        state.interval = Expr::FrameInterval(std::move(terms[0]), std::move(terms[1]));
      } else if (operation == "_time") {
        state.interval = Expr::TimeInterval(std::move(terms[0]), std::move(terms[1]));
      } else { // this should be unreachable
        utils::unreachable(
            "Interval Expression parsed an operation that was not _frame or _time");
      }
    } catch (const std::invalid_argument& e) {
      throw peg::parse_error(
          fmt::format("Invalid Interval Expression: {}", e.what()), in);
    }
  }
};

template <>
struct action<gm::quantifier_ops> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext&, Context& state) {
    auto content    = in.string();
    state.operation = std::move(content);
  }
};

template <>
struct action<gm::QuantifierExpression> {
  static void apply0(GlobalContext&, Context& state) {
    utils::assert_(
        state.operation.has_value(), "Expected a symbol for the quantifier operation");
    utils::assert_(
        state.variables.size() >= 1, "Expected at least 1 variable in the quantifier");
    utils::assert_(
        state.terms.size() == 1,
        "Expected exactly 1 sub-expression Term in the quantifier");
    std::string op_str = remove_opt_quick(state.operation);
    utils::assert_(
        op_str == "exists" || op_str == "forall", "Unknown quantifier operation");

    auto vars    = std::move(state.variables);
    auto terms   = std::move(state.terms);
    ExprPtr term = std::move(terms.back());

    if (op_str == "exists") {
      state.result = Expr::Exists(vars, term);
    } else { //  Must be forall
      state.result = Expr::Forall(vars, term);
    }
  }
};

template <>
struct action<gm::Operation> {
  static void apply0(GlobalContext&, Context& state) {
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
  static void handle_predicate(const ActionInput& in, Context& state, KnownOps op) {
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
  static void handle_unary(const ActionInput& in, Context& state, KnownOps op) {
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
    auto options     = std::move(state.attributes);
    auto options_set = AttrContainer{options.begin(), options.end()};
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
  static void handle_binary(const ActionInput& in, Context& state, KnownOps op) {
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
    auto options     = std::move(state.attributes);
    auto options_set = AttrContainer{options.begin(), options.end()};

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
        state.result = Expr::Sub(arg0, arg1);
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
  static void handle_nary(const ActionInput& in, Context& state, KnownOps op) {
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
  handle_temporal_unary(const ActionInput& in, Context& state, KnownOps op) {
    // 1. Check if we have at least 2 terms.
    if (state.terms.size() == 1) {
      throw peg::parse_error(
          fmt::format(
              "Temporal Unary operation expects exactly 1 argument, got {}",
              state.terms.size()),
          in);
    }
    // 2. Load the args
    auto terms                = std::move(state.terms);
    ExprPtr arg               = std::move(terms[0]);
    ast::IntervalPtr interval = std::move(state.interval);

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
  handle_temporal_binary(const ActionInput& in, Context& state, KnownOps op) {
    // 1. Check if we have 2 terms
    if (state.terms.size() == 2) {
      throw peg::parse_error(
          fmt::format(
              "Temporal Binary operation expects exactly 2 argument, got {}",
              state.terms.size()),
          in);
    }
    // 2. Load the args
    auto terms                = std::move(state.terms);
    ExprPtr arg0              = std::move(terms[0]);
    ExprPtr arg1              = std::move(terms[1]);
    ast::IntervalPtr interval = std::move(state.interval);

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
  static void apply(const ActionInput& in, GlobalContext&, Context& state) {
    utils::assert_(
        state.operation.has_value(), "Expected a symbol for the function operation");
    utils::assert_(state.terms.size() >= 1, "Expected at least 1 Term");

    std::string op_str         = remove_opt_quick(state.operation);
    std::optional<KnownOps> op = lookup(op_str);

    if (!op.has_value()) {
      auto options = AttrContainer{state.attributes.begin(), state.attributes.end()};
      state.result = Expr::ArithmeticFn(
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
    state.attributes.clear();
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
  static bool match(ParseInput& in, GlobalContext& global_state, Context& state) {
    // Here, we implement a push-down parser. Essentially, the new_state was
    // pushed onto the stack before the parser entered the rule for Term, and
    // now we have to pop the top of the stack (new_state) and merge the top
    // smartly with the old_state.

    // Create a new layer on the stack
    Context new_local_state{};
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
    // We also need to move the interval expression to the current context
    if (state.interval != nullptr && new_local_state.interval != nullptr) {
      throw peg::parse_error(
          "Multiple interval expressions defined for the same term", in);
    } else if (new_local_state.interval) {
      state.interval = std::move(new_local_state.interval);
    }

    return ret;
  }

  template <typename ActionInput>
  static void
  apply(const ActionInput& in, GlobalContext& global_state, Context& state) {
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
    } else if (state.identifier.has_value()) { // And if we have an id
      // Copy the pointer to the formula with the corresponding id
      auto id       = remove_opt_quick(state.identifier);
      const auto it = global_state.defined_formulas.find(id);
      if (it == global_state.defined_formulas.end()) {
        throw peg::parse_error(
            fmt::format("Reference to unknown identifier: `{}`", id), in);
      } else {
        state.terms.push_back(it->second);
      }
    } else if (!state.constants.empty()) { // we have constant
      auto constants = std::move(state.constants);
      utils::assert_(
          constants.size() == 1, "Term should have parsed exactly 1 Constant");
      if (std::holds_alternative<std::string>(constants.back())) {
        throw peg::parse_error("Strings currently cannot be terms", in);
      } else {
        ExprPtr c = utils::visit(
            utils::overloaded{
                [](const std::string&) { return Expr::Constant(/*constant*/ 0.0); },
                [](const auto& c) { return Expr::Constant(c); },
            },
            constants.back());
        state.terms.push_back(std::move(c));
      }
    } else {
      // Otherwise, it doesn't make sense that there are no results, as this
      // means that the parser failed. This should be unreachable.
      //
      // UPDATE 2021-01-26:
      //
      // Turns out, this is reachable if there is an empty "()" in the list of
      // expressions. This means that we need to throw a parsing error, where we
      // matched an empty list where it never makes sense to have one.
      throw peg::parse_error(
          "Looks like a pair '(' ')' was matched with nothing in between", in);
      // utils::unreachable("Looks like a Term has no sub expression or identifier.");
    }
  }
};

template <>
struct action<gm::CmdSetOption> {
  static void apply0(GlobalContext& ctx, Context& state) {
    utils::assert_(!state.attributes.empty(), "Expected at least 1 option");
    auto attrs = std::move(state.attributes);
    ctx.settings.insert(attrs.begin(), attrs.end());
  }
};

template <>
struct action<gm::CmdDefineFormula> {
  template <typename ActionInput>
  static void apply(const ActionInput& in, GlobalContext& ctx, Context& state) {
    utils::assert_(
        state.identifier.has_value() == 1,
        "Expected exactly 1 formula name identifier");

    auto terms = std::move(state.terms);
    auto expr  = std::move(terms[0]);
    auto id    = remove_opt_quick(state.identifier);

    const auto [it, not_overwrite] =
        ctx.defined_formulas.insert_or_assign(id, std::move(expr));
    if (!not_overwrite) {
      // We are overwriting a pre-defined formula. This is an error.
      throw peg::parse_error(
          fmt::format("Redefinition of existing command `{}`", id), in);
    }
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
  static bool match(ParseInput& in, GlobalContext& global_state, Context&) {
    // We create a new local state for each top-level command. This way, we don't have
    // any pollution of scopes between commands (due to dev error).
    Context state{};
    state.level = 0;

    return tao::pegtl::match<Rule, A, M, Action, Control>(in, global_state, state);
  }

  static void apply0(GlobalContext&, Context&) {
    // Kept empty to satisfy PEGTL
  }
};

} // namespace percemon::parser::actions

#endif /* end of include guard: PERCEMON_PARSER_ACTIONS_HPP */
