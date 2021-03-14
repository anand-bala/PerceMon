#include "percemon/ast/expression.hpp"       // for Interval, TemporalOp, Expr, ExprPtr
#include "utils/static_analysis_helpers.hpp" // for unreachable, assert_
#include "utils/visit.hpp"                   // for overloaded, visit

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

SpatialOp::SpatialOp(Type operation, std::vector<ExprPtr> arguments) :
    op{operation}, args{std::move(arguments)} {
  // Check if args have been initialized.
  if (args.empty()) {
    throw std::invalid_argument("A spatial function must have at least 1 argument");
  }

  // After the fields have been initialized, we need to check a few things.

  // First, we check the number of args.
  std::optional<size_t> nargs = {};
  switch (op) {
    // Unary
    case Type::BBox:
    case Type::Area:
    case Type::Complement:
    case Type::Interior:
    case Type::Closure:
      nargs = 1;
      break;
    case Type::Union:
    case Type::Intersect:
      nargs = std::nullopt;
      break;
  }
  if (nargs.has_value() && args.size() != *nargs) {
    throw std::invalid_argument(fmt::format(
        "Spatial operation ({}) expects {} arguments, got {}",
        magic_enum::enum_name(op),
        *nargs,
        args.size()));
  }

  // Second, we need to check if:
  //
  // 1. If the operation is BBox, then the expression is an ID Variable;
  // 2. Else, all arguments are Spatial operations .
  if (op == Type::BBox) {
    bool check = utils::visit(
        utils::overloaded{
            [](const Variable& var) { return var.is_object(); },
            [](const auto&) { return false; },
        },
        *args.back());
    if (!check) {
      throw std::invalid_argument(
          "Spatial function (BBox) only takes ID Variables are arguments");
    }
  } else {
    constexpr auto is_spatial_op = utils::overloaded{
        [](const SpatialOp&) { return true; },
        [](const SpatioTemporalOp&) { return true; },
        [](const auto&) { return false; },
    };

    for (const auto& [idx, arg] : ranges::views::enumerate(this->args)) {
      if (arg == nullptr) {
        throw std::invalid_argument("Arguments to Spatial function cannot be null");
      }
      bool check = utils::visit(is_spatial_op, *arg);
      if (!check) {
        throw std::invalid_argument(fmt::format(
            "Argument at position {} for Spatial function is invalid", idx + 1));
      }
    }
  }
}

std::string SpatialOp::to_string() const {
  auto op_name = std::string{magic_enum::enum_name(op)};
  // convert to lower-case
  std::transform(op_name.begin(), op_name.end(), op_name.begin(), [](unsigned char c) {
    return std::tolower(c);
  });

  auto args_str = std::vector<std::string>{};
  args_str.reserve(args.size());
  for (const auto& arg : args) { args_str.push_back(arg->to_string()); }
  return fmt::format("({} {})", op_name, fmt::join(args_str, " "));
}

SpatioTemporalOp::SpatioTemporalOp(
    Type operation,
    std::vector<ExprPtr> arguments,
    std::shared_ptr<Interval> interval_arg) :
    op{operation}, args{std::move(arguments)}, interval{std::move(interval_arg)} {
  // We need to check the number of arguments.
  std::optional<size_t> nargs = {};
  // Also check if an interval is present when there shouldn't be one
  bool can_have_interval = true;
  switch (op) {
    case Type::Next:
    case Type::Previous:
      can_have_interval = false;
      nargs             = 1;
      break;
    case Type::Eventually:
    case Type::Once:
    case Type::Always:
    case Type::Historically:
      can_have_interval = true;
      nargs             = 1;
      break;
    case Type::Until:
    case Type::Since:
      can_have_interval = true;
      nargs             = 2;
      break;
  }
  if (!can_have_interval && interval != nullptr) {
    throw std::invalid_argument(fmt::format(
        "Temporal operation {} cannot have an interval", magic_enum::enum_name(op)));
  }
  if (nargs.has_value() && *nargs != args.size()) {
    throw std::invalid_argument(fmt::format(
        "Temporal operation {} expected {} arguments, got {}",
        magic_enum::enum_name(op),
        *nargs,
        args.size()));
  }

  // Now we also need to check if the arguments are either SpatialOp or SpatioTemporalOp
  constexpr auto is_spatial_op = utils::overloaded{
      [](const SpatialOp&) { return true; },
      [](const SpatioTemporalOp&) { return true; },
      [](const auto&) { return false; },
  };

  for (const auto& [idx, arg] : ranges::views::enumerate(this->args)) {
    if (arg == nullptr) {
      throw std::invalid_argument(
          "Arguments to Spatio-temporal function cannot be null");
    }
    bool check = utils::visit(is_spatial_op, *arg);
    if (!check) {
      throw std::invalid_argument(fmt::format(
          "Argument at position {} for Spatio-temporal function is invalid", idx + 1));
    }
  }
}

std::string SpatioTemporalOp::to_string() const {
  auto op_name = std::string{magic_enum::enum_name(op)};
  // convert to lower-case
  std::transform(op_name.begin(), op_name.end(), op_name.begin(), [](unsigned char c) {
    return std::tolower(c);
  });
  // Add the "spatial-" prefix to this.
  std::string op_str = std::string{"spatial-"} + std::string(op_name);
  // Get the arguments as strings
  auto args_str = std::vector<std::string>(args.size());
  for (const auto& sub_expr : args) { args_str.push_back(sub_expr->to_string()); }
  // Get the interval as a string
  auto interval_str = (interval == nullptr) ? "" : interval->to_string();
  return fmt::format("({} {} {})", op_str, interval_str, fmt::join(args_str, " "));
}

SpatialQuantifier::SpatialQuantifier(Type operation, ExprPtr argument) :
    op{operation}, arg{std::move(argument)} {
  // Make sure the arg is not null
  if (arg == nullptr) {
    throw std::invalid_argument("Spatial Quantifier argument cannot be null");
  }

  // Check the type of the argument
  bool is_spatial_arg = utils::visit(
      utils::overloaded{
          [](const SpatialOp&) { return true; },
          [](const SpatioTemporalOp&) { return true; },
          [](const auto&) { return false; },
      },
      arg);
  if (!is_spatial_arg) {
    throw std::invalid_argument(
        "Spatial quantifier must have a Spatial or Spatio-temporal operation");
  }
}

std::string SpatialQuantifier::to_string() const {
  auto op_name = std::string{magic_enum::enum_name(op)};
  // convert to lower-case
  std::transform(op_name.begin(), op_name.end(), op_name.begin(), [](unsigned char c) {
    return std::tolower(c);
  });
  // Add the "spatial-" prefix to this.
  std::string op_str = std::string{"spatial-"} + std::string(op_name);
  // Get the argument as string
  auto arg_str = arg->to_string();
  return fmt::format("({} {})", op_name, arg_str);
}

} // namespace ast::details

ExprPtr Expr::BBox(ExprPtr obj) {
  return make_expr(ast::details::SpatialOp{ast::SpatialOpType::BBox, {std::move(obj)}});
}

ExprPtr Expr::Area(ExprPtr obj) {
  return make_expr(ast::details::SpatialOp{ast::SpatialOpType::Area, {std::move(obj)}});
}

ExprPtr Expr::Complement(ExprPtr arg) {
  return make_expr(
      ast::details::SpatialOp{ast::SpatialOpType::Complement, {std::move(arg)}});
}

ExprPtr Expr::Union(ExprPtrContainer arg) {
  return make_expr(ast::details::SpatialOp{ast::SpatialOpType::Union, std::move(arg)});
}

ExprPtr Expr::Intersect(ExprPtrContainer arg) {
  return make_expr(
      ast::details::SpatialOp{ast::SpatialOpType::Intersect, std::move(arg)});
}

ExprPtr Expr::SpatialNext(ExprPtr arg) {
  return make_expr(
      ast::details::SpatioTemporalOp{ast::SpModalOpType::Next, {std::move(arg)}});
}

ExprPtr Expr::SpatialPrevious(ExprPtr arg) {
  return make_expr(
      ast::details::SpatioTemporalOp{ast::SpModalOpType::Previous, {std::move(arg)}});
}

ExprPtr Expr::SpatialEventually(ExprPtr arg) {
  return make_expr(
      ast::details::SpatioTemporalOp{ast::SpModalOpType::Eventually, {std::move(arg)}});
}

ExprPtr Expr::SpatialEventually(ExprPtr arg, std::shared_ptr<ast::Interval> interval) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Eventually, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::SpatialOnce(ExprPtr arg) {
  return make_expr(
      ast::details::SpatioTemporalOp{ast::SpModalOpType::Once, {std::move(arg)}});
}

ExprPtr Expr::SpatialOnce(ExprPtr arg, std::shared_ptr<ast::Interval> interval) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Once, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::SpatialAlways(ExprPtr arg) {
  return make_expr(
      ast::details::SpatioTemporalOp{ast::SpModalOpType::Always, {std::move(arg)}});
}

ExprPtr Expr::SpatialAlways(ExprPtr arg, std::shared_ptr<ast::Interval> interval) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Always, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::SpatialHistorically(ExprPtr arg) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Historically, {std::move(arg)}});
}

ExprPtr
Expr::SpatialHistorically(ExprPtr arg, std::shared_ptr<ast::Interval> interval) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Historically, {std::move(arg)}, std::move(interval)});
}

ExprPtr Expr::SpatialUntil(ExprPtr arg1, ExprPtr arg2) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Until, {std::move(arg1), std::move(arg2)}});
}

ExprPtr Expr::SpatialUntil(
    ExprPtr arg1,
    ExprPtr arg2,
    std::shared_ptr<ast::Interval> interval) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Until,
      {std::move(arg1), std::move(arg2)},
      std::move(interval)});
}

ExprPtr Expr::SpatialSince(ExprPtr arg1, ExprPtr arg2) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Since, {std::move(arg1), std::move(arg2)}});
}

ExprPtr Expr::SpatialSince(
    ExprPtr arg1,
    ExprPtr arg2,
    std::shared_ptr<ast::Interval> interval) {
  return make_expr(ast::details::SpatioTemporalOp{
      ast::SpModalOpType::Since,
      {std::move(arg1), std::move(arg2)},
      std::move(interval)});
}

ExprPtr Expr::NotEmpty(ExprPtr arg) {
  return make_expr(ast::details::SpatialQuantifier{
      ast::SpatialQuantifierType::Exists, std::move(arg)});
}

ExprPtr Expr::Fills(ExprPtr arg) {
  return make_expr(ast::details::SpatialQuantifier{
      ast::SpatialQuantifierType::Forall, std::move(arg)});
}

} // namespace percemon
