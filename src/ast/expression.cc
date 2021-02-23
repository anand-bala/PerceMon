#include "percemon/ast/expression.hpp"
#include "percemon/exception.hpp"

#include "utils/utils.hpp"

#include <atomic>
#include <cstddef>
#include <memory>

#include <magic_enum.hpp>

/// Counter for `Expr`
static std::size_t get_next_id() {
  static std::atomic_size_t id = 0;
  return id++;
}

namespace percemon {
namespace nodes = PERCEMON_AST_NS;

using CmpOp             = nodes::PredicateOp::Type;
using BoolOp            = nodes::LogicalOp::Type;
using Quantifier        = nodes::QuantifierOp::Type;
using TempOp            = nodes::TemporalOp::Type;
using SpaceOp           = nodes::SpatialOp::Type;
using SpaceQuantifierOp = nodes::SpatialQuantifier::Type;

template <typename ExprType>
std::unique_ptr<Expr> Expr::make_expr(ExprType arg) {
  auto expr = std::make_unique<Expr>(std::move(arg));
  expr->id  = get_next_id();
  return expr;
}

template <typename CType>
std::unique_ptr<Expr> Expr::Constant(CType c) {
  return make_expr(nodes::Constant{c});
}

std::unique_ptr<Expr> Expr::Var_f(std::string name) {
  return make_expr(nodes::Variable{std::move(name), ast::VarType::Frame});
}

std::unique_ptr<Expr> Expr::Var_t(std::string name) {
  return make_expr(nodes::Variable{std::move(name), ast::VarType::Timepoint});
}

std::unique_ptr<Expr> Expr::Variable(std::string name, ast::VarType type) {
  return make_expr(nodes::Variable{std::move(name), type});
}

std::unique_ptr<Expr> Expr::Variable(std::string name, std::string type_str) {
  auto custom_type = std::optional<std::string>{};
  auto type_opt    = magic_enum::enum_cast<ast::VarType>(name);
  auto type        = ast::VarType::Unknown;
  if (type_opt.has_value()) {
    type = *type_opt;
  } else { // Custom
    type        = ast::VarType::Custom;
    custom_type = std::move(type_str);
  }

  return make_expr(nodes::Variable{std::move(name), type, std::move(custom_type)});
}

std::unique_ptr<Expr>
Expr::Function(ast::FnType op, ExprPtrContainer args, AttrContainer attrs) {
  return make_expr(nodes::Function{op, std::move(args), std::move(attrs)});
}

std::unique_ptr<Expr>
Expr::Function(std::string op, ExprPtrContainer args, AttrContainer attrs) {
  auto custom_fn = std::optional<std::string>{};
  auto fn_opt    = magic_enum::enum_cast<ast::FnType>(op);

  std::unique_ptr<Expr> expr = nullptr;
  if (fn_opt.has_value()) {
    return make_expr(nodes::Function{*fn_opt, std::move(args), std::move(attrs)});
  } else {
    return make_expr(nodes::Function{std::move(op), std::move(args), std::move(attrs)});
  }
}

std::unique_ptr<Expr> Expr::Eq(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{CmpOp::EQ, std::move(lhs), std::move(rhs)});
}

std::unique_ptr<Expr> Expr::Neq(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{CmpOp::NE, std::move(lhs), std::move(rhs)});
}

std::unique_ptr<Expr> Expr::Lt(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{CmpOp::LT, std::move(lhs), std::move(rhs)});
}

std::unique_ptr<Expr> Expr::Le(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{CmpOp::LE, std::move(lhs), std::move(rhs)});
}

std::unique_ptr<Expr> Expr::Gt(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{CmpOp::GT, std::move(lhs), std::move(rhs)});
}

std::unique_ptr<Expr> Expr::Ge(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{CmpOp::GE, std::move(lhs), std::move(rhs)});
}

std::unique_ptr<Expr> Expr::Not(ExprPtr arg) {
  return make_expr(nodes::LogicalOp{BoolOp::Not, {std::move(arg)}});
}

std::unique_ptr<Expr> Expr::And(ExprPtrContainer arg) {
  return make_expr(nodes::LogicalOp{BoolOp::And, std::move(arg)});
}

std::unique_ptr<Expr> Expr::Or(ExprPtrContainer arg) {
  return make_expr(nodes::LogicalOp{BoolOp::Or, std::move(arg)});
}

std::unique_ptr<Expr> Expr::Implies(ExprPtr x, ExprPtr y) {
  ExprPtr not_x = Not(std::move(x));
  return Or({std::move(not_x), std::move(y)});
}

std::unique_ptr<Expr> Expr::Xor(ExprPtr x, ExprPtr y) {
  ExprPtr not_x    = Not(x);
  ExprPtr not_y    = Not(y);
  ExprPtr x_or_y   = Or({std::move(x), std::move(y)});
  ExprPtr nx_or_ny = Or({std::move(not_x), std::move(not_y)});
  return And({std::move(x_or_y), std::move(nx_or_ny)});
}

std::unique_ptr<Expr> Expr::Iff(ExprPtr x, ExprPtr y) {
  ExprPtr not_x     = Not(x);
  ExprPtr not_y     = Not(y);
  ExprPtr x_and_y   = And({std::move(x), std::move(y)});
  ExprPtr nx_and_ny = And({std::move(not_x), std::move(not_y)});
  return Or({std::move(x_and_y), std::move(nx_and_ny)});
}

std::unique_ptr<Expr> Expr::Exists(ExprPtrContainer vars, ExprPtr arg) {
  return make_expr(
      nodes::QuantifierOp{Quantifier::Exists, std::move(vars), std::move(arg)});
}

std::unique_ptr<Expr> Expr::Forall(ExprPtrContainer vars, ExprPtr arg) {
  return make_expr(
      nodes::QuantifierOp{Quantifier::Forall, std::move(vars), std::move(arg)});
}

std::unique_ptr<Expr> Expr::Next(ExprPtr arg) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(nodes::TemporalOp{TempOp::Next, std::move(arg_array)});
}

std::unique_ptr<Expr> Expr::Previous(ExprPtr arg) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(nodes::TemporalOp{TempOp::Previous, std::move(arg_array)});
}

std::unique_ptr<Expr> Expr::Eventually(ExprPtr arg, ExprPtr interval) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(
      nodes::TemporalOp{TempOp::Eventually, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::Once(ExprPtr arg, ExprPtr interval) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(
      nodes::TemporalOp{TempOp::Once, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::Always(ExprPtr arg, ExprPtr interval) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(
      nodes::TemporalOp{TempOp::Always, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::Historically(ExprPtr arg, ExprPtr interval) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(nodes::TemporalOp{
      TempOp::Historically, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::Until(ExprPtr arg1, ExprPtr arg2, ExprPtr interval) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg1), std::move(arg2)};
  return make_expr(
      nodes::TemporalOp{TempOp::Until, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::Since(ExprPtr arg1, ExprPtr arg2, ExprPtr interval) {
  std::array<ExprPtr, 2> arg_array = {std::move(arg1), std::move(arg2)};
  return make_expr(
      nodes::TemporalOp{TempOp::Since, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::Complement(ExprPtr arg) {
  std::vector<ExprPtr> args = {std::move(arg)};
  return make_expr(nodes::SpatialOp{SpaceOp::Complement, std::move(args)});
}

std::unique_ptr<Expr> Expr::Union(ExprPtrContainer args) {
  return make_expr(nodes::SpatialOp{SpaceOp::Union, std::move(args)});
}

std::unique_ptr<Expr> Expr::Intersect(ExprPtrContainer args) {
  return make_expr(nodes::SpatialOp{SpaceOp::Intersect, std::move(args)});
}

std::unique_ptr<Expr> Expr::NotEmpty(ExprPtr arg) {
  return make_expr(
      nodes::SpatialQuantifier{SpaceQuantifierOp::SpExists, std::move(arg)});
}

std::unique_ptr<Expr> Expr::Fills(ExprPtr arg) {
  return make_expr(
      nodes::SpatialQuantifier{SpaceQuantifierOp::SpForall, std::move(arg)});
}

std::unique_ptr<Expr> Expr::SpatialNext(ExprPtr arg) {
  auto type                        = nodes::SpatioTemporalOp::Type::Next;
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(nodes::SpatioTemporalOp{type, std::move(arg_array)});
}

std::unique_ptr<Expr> Expr::SpatialPrevious(ExprPtr arg) {
  auto type                        = nodes::SpatioTemporalOp::Type::Previous;
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(nodes::SpatioTemporalOp{type, std::move(arg_array)});
}

std::unique_ptr<Expr> Expr::SpatialEventually(ExprPtr arg, ExprPtr interval) {
  auto type                        = nodes::SpatioTemporalOp::Type::Eventually;
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(
      nodes::SpatioTemporalOp{type, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::SpatialOnce(ExprPtr arg, ExprPtr interval) {
  auto type                        = nodes::SpatioTemporalOp::Type::Once;
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(
      nodes::SpatioTemporalOp{type, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::SpatialAlways(ExprPtr arg, ExprPtr interval) {
  auto type                        = nodes::SpatioTemporalOp::Type::Always;
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(
      nodes::SpatioTemporalOp{type, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::SpatialHistorically(ExprPtr arg, ExprPtr interval) {
  auto type                        = nodes::SpatioTemporalOp::Type::Historically;
  std::array<ExprPtr, 2> arg_array = {std::move(arg), nullptr};
  return make_expr(
      nodes::SpatioTemporalOp{type, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::SpatialUntil(ExprPtr arg1, ExprPtr arg2, ExprPtr interval) {
  auto type                        = nodes::SpatioTemporalOp::Type::Until;
  std::array<ExprPtr, 2> arg_array = {std::move(arg1), std::move(arg2)};
  return make_expr(
      nodes::SpatioTemporalOp{type, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::SpatialSince(ExprPtr arg1, ExprPtr arg2, ExprPtr interval) {
  auto type                        = nodes::SpatioTemporalOp::Type::Since;
  std::array<ExprPtr, 2> arg_array = {std::move(arg1), std::move(arg2)};
  return make_expr(
      nodes::SpatioTemporalOp{type, std::move(arg_array), std::move(interval)});
}

std::unique_ptr<Expr> Expr::PinAt(ExprPtr time_var, ExprPtr frame_var) {
  return make_expr(nodes::PinnedFrame{std::move(time_var), std::move(frame_var)});
}

std::unique_ptr<Expr> Expr::PinAtFrame(ExprPtr frame_var) {
  return PinAt(nullptr, std::move(frame_var));
}

std::unique_ptr<Expr> Expr::PinAtTime(ExprPtr time_var) {
  return PinAt(std::move(time_var), nullptr);
}

std::unique_ptr<Expr> Expr::Dist(ExprPtr x, ExprPtr y, AttrContainer attrs) {
  return Function(ast::FnType::Dist, {std::move(x), std::move(y)}, std::move(attrs));
}

std::unique_ptr<Expr> Expr::Offset(ExprPtr arg, AttrContainer attrs) {
  return Function(ast::FnType::Offset, {std::move(arg)}, std::move(attrs));
}

std::unique_ptr<Expr> Expr::Class(ExprPtr obj) {
  return Function(ast::FnType::Dist, {std::move(obj)}, {});
}

std::unique_ptr<Expr> Expr::Prob(ExprPtr obj) {
  return Function(ast::FnType::Prob, {std::move(obj)}, {});
}

std::unique_ptr<Expr> Expr::Area(ExprPtr obj) {
  return Function(ast::FnType::Area, {std::move(obj)}, {});
}

std::unique_ptr<Expr> Expr::BBox(ExprPtr obj) {
  return Function(ast::FnType::BBox, {std::move(obj)}, {});
}

std::unique_ptr<Expr> Expr::Add(ExprPtrContainer args) {
  return Function(ast::FnType::Add, std::move(args), {});
}

std::unique_ptr<Expr> Expr::Mul(ExprPtrContainer args) {
  return Function(ast::FnType::Mul, std::move(args), {});
}

std::unique_ptr<Expr> Expr::Subtract(ExprPtr lhs, ExprPtr rhs) {
  return Function(ast::FnType::Add, {std::move(lhs), std::move(rhs)}, {});
}

std::unique_ptr<Expr> Expr::Div(ExprPtr numerator, ExprPtr denominator) {
  return Function(ast::FnType::Div, {std::move(numerator), std::move(denominator)}, {});
}

} // namespace percemon
