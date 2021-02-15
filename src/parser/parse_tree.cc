#include "parse_tree.hpp"

#include "percemon/ast/expression.hpp"
#include "percemon/parser.hpp"

#include "utils/static_analysis_helpers.hpp"

#include <fmt/format.h>
#include <tao/pegtl/contrib/parse_tree.hpp>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view.hpp>

#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string_view>

namespace peg = tao::pegtl;
namespace gm  = percemon::grammar;
using NodePtr = std::unique_ptr<peg::parse_tree::node>;
using percemon::parser::parse_tree::ParseContext;

namespace {

void make_ast(const std::unique_ptr<peg::parse_tree::node>& node, ParseContext& stack);
percemon::ExprPtr get_term(const NodePtr& node, ParseContext& stack);

std::string get_symbol(const NodePtr& node, ParseContext& stack) {
  // Here, we know that node is a symbol. Hence, we need to
  //
  // 1. If it is a QuotedSymbol, trim `|` on both ends.
  // 2. Save the symbol.
  utils::assert_(node->has_content(), "Symbol node doesn't have content");
  utils::assert_(
      node->children.size() == 1, "Symbol node needs to have exactly 1 child");

  // Check if QuotedSymbol
  bool is_quoted = node->children.front()->is_type<gm::QuotedSymbol>();

  auto content_rng = node->string_view() |
                     ranges::views::trim([=](char c) { return is_quoted && c == '|'; });

  std::string symbol = ranges::to<std::string>(content_rng);
  stack.symbols.insert(symbol);
  return symbol;
}

percemon::ExprPtr get_const(const NodePtr& node, ParseContext& stack) {
  utils::assert_(node->has_content(), "Constant node doesn't have content");
  utils::assert_(
      node->children.size() == 1, "Constant node needs to have exactly 1 child");

  auto& child  = node->children.front();
  auto content = child->string();

  auto it = stack.lut.find(content);
  if (it != stack.lut.end()) { // Already defined expr
    return it->second;
  }

  auto expr = percemon::ExprPtr{nullptr};
  if (child->is_type<gm::DoubleLiteral>()) {
    double c = std::stod(content);
    expr     = percemon::Expr::Constant(c);
  } else if (child->is_type<gm::IntegerLiteral>()) {
    int c = std::stoi(content);
    expr  = percemon::Expr::Constant(c);
  } else if (child->is_type<gm::BooleanLiteral>()) {
    bool c = content == "true";
    expr   = percemon::Expr::Constant(c);
  } else { // string
    expr = percemon::Expr::Constant(content);
  }

  // Add Expr to LUT
  stack.lut.insert({content, expr});

  return expr;
}

percemon::ExprPtr get_expr(const NodePtr& node, ParseContext& stack) {
  // Can have either 1 children or many.
  utils::assert_(
      node->children.size() >= 1, "Expression node must contain at least 1 child node");

  {
    auto it = stack.lut.find(node->string());
    if (it != stack.lut.end()) { // Already defined expr
      return it->second;
    }
  }

  if (node->children.size() == 1) { // May be special expression
    auto& child = node->children.front();
    if (child->is_type<gm::PinningExpression>()) {
      return get_pinned_frame(child, stack);
    } else if (child->is_type<gm::QuantifierExpression>()) {
      return get_quantifier(child, stack);
    }
    utils::unreachable();
  }

  using namespace ranges;
  // Can be some arbitrary function
  auto expr = percemon::ExprPtr{nullptr};

  const auto& children = node->children;
  size_t n_children    = children.size();
  utils::assert_(
      n_children >= 2,
      "Function node must have 1 operation symbol and at least 1 operand");

  std::string op = children.at(0)->string();
  auto it        = std::next(children.cbegin());

  std::vector<percemon::ExprPtr> terms;
  for (; it != children.cend() && (*it)->is_type<gm::Term>(); it++) {}
  utils::assert_(std::size(terms) >= 1, "Function must have at least 1 operand");
  return expr;
}

percemon::ExprPtr get_term(const NodePtr& node, ParseContext& stack) {
  utils::assert_(node->children.size() == 1, "Term needs to have exactly 1 child");

  const auto& child = node->children.front();
  auto term         = percemon::ExprPtr{nullptr};
  if (child->is_type<gm::QualifiedIdentifier>()) {
    auto symbol = get_symbol(child, stack);
    auto lut_it = stack.lut.find(symbol);
    if (lut_it != stack.lut.end()) {
      term = lut_it->second;
    } else {
      throw peg::parse_error(
          fmt::format("Reference to undefined symbol {}", symbol), node->begin());
    }
  } else if (child->is_type<gm::Constant>()) {
    term = get_const(child, stack);
  } else { // Must be an Expression
    term = get_expr(child, stack);
  }
  // Check if term is valid
  if (!term->is_valid()) {
    throw peg::parse_error(
        fmt::format("Term is syntactically incorrect"), node->begin());
  }
  return term;
}

void make_ast(const std::unique_ptr<peg::parse_tree::node>& node, ParseContext& stack) {
  if (node->is_root()) {
    return make_ast(node->children.front(), stack);
  }

  if (node->is_type<gm::Specification>()) {
    return make_ast(node->children.front(), stack);
  } else if (node->is_type<gm::StatementList>()) {
    std::for_each(
        std::cbegin(node->children), std::cend(node->children), [&](const auto& child) {
          make_ast(child, stack);
        });
  } else if (node->is_type<gm::CmdDefineFormula>()) {
    auto symbol = get_symbol(node->children.at(0), stack);
    auto term   = get_term(node->children.at(1), stack);

    const auto [it, success] = stack.defined_formulas.insert({symbol, term});
    if (!success) { // formula was already defined
      throw peg::parse_error(
          fmt::format("[define-formula] Redefinition of formula named `{}`", symbol),
          node->begin());
    }
  } else if (node->is_type<gm::CmdMonitor>()) {
    // TODO: What are the attributes? We may need to create specialized Monitor classes.
    auto symbol = get_symbol(node->children.at(0), stack);
    auto term   = get_term(node->children.at(1), stack);

    const auto [it, success] = stack.monitors.insert({symbol, term});
    if (!success) { // formula was already defined
      throw peg::parse_error(
          fmt::format("[monitor] Redefinition of monitor named `{}`", symbol),
          node->begin());
    }
  } else if (node->is_type<gm::CmdSetOption>()) {
    // TODO
    fmt::print(stderr, "Warning: [set-option] Command not used for now");
  }
  // TODO: I think only top level commands need to be handled here, as everything else
  // recusrively sorts out.
  return; // By default
}

} // namespace

namespace percemon::parser::parse_tree {

std::unique_ptr<ParseContext> transform(NodePtr root) {
  auto stack = ParseContext();
  make_ast(root, stack);
  auto ctx = std::make_unique<ParseContext>();
  return ctx;
}

} // namespace percemon::parser::parse_tree
