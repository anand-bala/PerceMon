#include "percemon/ast/expression.hpp" // for Interval, TemporalOp, Expr, ExprPtr
#include "utils/visit.hpp"             // for overloaded, visit

#include <cstddef>     // for size_t
#include <limits>      // for numeric_limits
#include <memory>      // for __shared_ptr_access, shared_ptr
#include <stdexcept>   // for invalid_argument
#include <string>      // for string
#include <string_view> // for string_view
#include <type_traits> // for is_arithmetic
#include <utility>     // for move
#include <variant>     // for holds_alternative, get
#include <vector>      // for vector

#include <fmt/format.h>                // for format, join
#include <magic_enum.hpp>              // for enum_name
#include <range/v3/view/enumerate.hpp> // for enumerate, enumerate_fn
#include <range/v3/view/view.hpp>      // for view_closure

namespace percemon {

namespace ast::details {

TemporalOp::TemporalOp(
    Type operation,
    std::vector<ExprPtr> arguments,
    std::shared_ptr<Interval> interval_arg) :
    op{operation}, args{std::move(arguments)}, interval{std::move(interval_arg)} {
  // Here, we need to check:
  //
  // 1. If the type is Unary, then the number of arguments is 1. Otherwise 2.
  // 2. If the type is Prev or Next, it must not contain an Interval.
  // 3. Each argument must be a Predicate, a LogicalOp, or a TemporalOp

  // 1. Check nargs
  size_t required_args = 2;
  switch (op) {
    case Type::Since:
    case Type::Until:
      required_args = 2;
      break;
    default:
      required_args = 1;
      break;
  }
  if (required_args != args.size()) {
    throw std::invalid_argument(fmt::format(
        "Operation `{}` requires exactly {} arguments, got {}.",
        magic_enum::enum_name(op),
        required_args,
        args.size()));
  }

  // 2. Check argument type
  for (const auto& [idx, expr] : ranges::views::enumerate(args)) {
    if (expr == nullptr) {
      throw std::invalid_argument(fmt::format(
          "[{}] Argument at position {} is null", magic_enum::enum_name(op), idx));
    }
    bool is_valid = utils::visit(
        utils::overloaded{
            [](const Constant&) { return false; },
            [](const Variable&) { return false; },
            [](const Function&) { return false; },
            [](const SpatialOp&) { return false; },
            [](const SpatioTemporalOp&) { return false; },
            [](const auto&) { // Can be anything but the above.
              return true;
            },
        },
        *expr);
    if (!is_valid) {
      throw std::invalid_argument(fmt::format(
          "Argument at position {} is not valid: must be a Predicate, a Logical Operation, a Temporal Operation, or a Quantifier.",
          idx));
    }
  }
}

std::string TemporalOp::to_string() const {
  auto op_name = std::string{magic_enum::enum_name(op)};
  // convert to lower-case
  std::transform(op_name.begin(), op_name.end(), op_name.begin(), [](unsigned char c) {
    return std::tolower(c);
  });
  auto args_str = std::vector<std::string>(args.size());
  for (const auto& sub_expr : args) { args_str.push_back(sub_expr->to_string()); }

  auto interval_str = (interval == nullptr) ? "" : interval->to_string();
  return fmt::format("({} {} {})", op_name, interval_str, fmt::join(args_str, " "));
}
} // namespace ast::details

ExprPtr Expr::Next(ExprPtr arg) {
  return make_expr(
      percemon::ast::details::TemporalOp{ast::ModalOpType::Next, {std::move(arg)}});
}

ExprPtr Expr::Previous(ExprPtr arg) {
  return make_expr(
      percemon::ast::details::TemporalOp{ast::ModalOpType::Previous, {std::move(arg)}});
}

ExprPtr Expr::Eventually(ExprPtr arg) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Eventually, {std::move(arg)}});
}

ExprPtr Expr::Eventually(
    ExprPtr arg,
    std::shared_ptr<percemon::ast::details::Interval> interval) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Eventually, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::Once(ExprPtr arg) {
  return make_expr(
      percemon::ast::details::TemporalOp{ast::ModalOpType::Once, {std::move(arg)}});
}

ExprPtr
Expr::Once(ExprPtr arg, std::shared_ptr<percemon::ast::details::Interval> interval) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Once, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::Always(ExprPtr arg) {
  return make_expr(
      percemon::ast::details::TemporalOp{ast::ModalOpType::Always, {std::move(arg)}});
}

ExprPtr
Expr::Always(ExprPtr arg, std::shared_ptr<percemon::ast::details::Interval> interval) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Always, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::Historically(ExprPtr arg) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Historically, {std::move(arg)}});
}

ExprPtr Expr::Historically(
    ExprPtr arg,
    std::shared_ptr<percemon::ast::details::Interval> interval) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Historically, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::Until(ExprPtr arg1, ExprPtr arg2) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Until, {std::move(arg1), std::move(arg2)}});
}

ExprPtr Expr::Until(
    ExprPtr arg1,
    ExprPtr arg2,
    std::shared_ptr<percemon::ast::details::Interval> interval) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Until,
      {std::move(arg1), std::move(arg2)},
      std::move(interval)});
}

ExprPtr Expr::Since(ExprPtr arg1, ExprPtr arg2) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Since, {std::move(arg1), std::move(arg2)}});
}

ExprPtr Expr::Since(
    ExprPtr arg1,
    ExprPtr arg2,
    std::shared_ptr<percemon::ast::details::Interval> interval) {
  return make_expr(percemon::ast::details::TemporalOp{
      ast::ModalOpType::Since,
      {std::move(arg1), std::move(arg2)},
      std::move(interval)});
}
} // namespace percemon
