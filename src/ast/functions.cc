#include "percemon/ast/expression.hpp"       // for ExprPtr, Attribute, Function
#include "utils/static_analysis_helpers.hpp" // for assert_
#include "utils/visit.hpp"                   // for overloaded, visit

#include <fmt/format.h>   // for format, join
#include <magic_enum.hpp> // for enum_name, enum_cast

#include <memory>      // for allocator, operator==
#include <optional>    // for optional
#include <set>         // for set
#include <stdexcept>   // for invalid_argument
#include <string>      // for string, basic_string
#include <string_view> // for string_view
#include <utility>     // for move
#include <vector>      // for vector

namespace percemon {

namespace ast::details {

Function::Function(
    Type op,
    std::optional<std::string> op_str,
    std::vector<ExprPtr> operands,
    std::set<Attribute, Attribute::KeyCompare> attributes) :
    fn{op},
    custom_fn{std::move(op_str)},
    args{std::move(operands)},
    attrs{std::move(attributes)} {
  // Check if args have been initialized.
  utils::assert_(!args.empty(), "A function must have at least 1 argument");
  // After the fields have been initialized, we need to check a few things.
  // 1. If the Type is not custom, the custom_fn field must be empty:
  if (custom_fn.has_value() && fn != Type::Custom) {
    throw std::invalid_argument(fmt::format(
        "Function having known type `{}` was also supplied with a custom function name string \"{}\"",
        magic_enum::enum_name(fn),
        *custom_fn));
  }
  // 2. If type is custom, but the string is empty
  if (fn == Type::Custom) {
    if (!custom_fn.has_value() || *custom_fn == "") {
      /// This should never happen when using the parser.
      throw std::invalid_argument(
          "Function with custom operation has an empty operation name");
    }
  }
  // 3. Validate
  validate();
}

void Function::validate() const {
  // First, we check the number of args.
  std::optional<size_t> nargs = {};
  switch (fn) {
    // Unary
    case Type::Class:
    case Type::Prob:
    case Type::Offset:
      nargs = 1;
      break;
    // Binary
    case Type::Dist:
    case Type::Div:
    case Type::Sub:
      nargs = 2;
      break;
    case Type::Add:
    case Type::Mul:
    case Type::Custom:
      nargs = std::nullopt;
      break;
  }
  if (nargs.has_value() && nargs.value() != args.size()) {
    throw std::invalid_argument(fmt::format(
        "The number of arguments given to the function (nargs = {}) does not match the expected number (expected = {}).",
        args.size(),
        nargs.value()));
  }

  // Now we can check the functions.
  if (is_object_op()) {
    // If the function is defined for object IDs, we need to check if all operands are
    // ID Variables.
    auto is_valid = utils::overloaded{
        [](const Variable& var) { return var.is_frame() || var.is_timepoint(); },
        [](const auto&) { return false; },
    };

    for (const auto& expr : this->args) {
      bool check = utils::visit(is_valid, *expr);
      if (!check) {
        throw std::invalid_argument(fmt::format(
            "Function on object variables ({}) can only take ID Variables as arguments",
            magic_enum::enum_name(fn)));
      }
    }
  } else {
    // Otherwise, we just need to check if all operands are either Constants, Variables,
    // or Functions.
    auto is_valid = utils::overloaded{
        [](const Constant&) { return true; },
        [](const Variable&) { return true; },
        [](const Function&) { return true; },
        [](const auto&) {
          return false;
        }};

    for (const auto& expr : this->args) {
      bool check = utils::visit(is_valid, *expr);
      if (!check) {
        throw std::invalid_argument(
            "Functions can only take Constants, Variables, or other Functions arguments");
      }
    }
  }
}

std::string Function::to_string() const {
  std::string_view op;
  switch (fn) {
    case Type::Custom:
      op = custom_fn.value();
      break;
    case Type::Add:
      op = "+";
      break;
    case Type::Sub:
      op = "-";
      break;
    case Type::Mul:
      op = "*";
      break;
    case Type::Div:
      op = "/";
      break;
    case Type::Dist:
      op = "dist";
      break;
    case Type::Offset:
      op = "offset";
      break;
    case Type::Class:
      op = "class";
      break;
    case Type::Prob:
      op = "prob";
      break;
  }

  auto args_str = std::vector<std::string>(args.size());
  for (const auto& sub_expr : args) { args_str.push_back(sub_expr->to_string()); }
  auto attr_str = std::vector<std::string>(attrs.size());
  for (const auto& attr : attrs) { attr_str.push_back(attr.to_string()); }

  return fmt::format(
      "({} {} {})", op, fmt::join(args_str, " "), fmt::join(attr_str, " "));
}

bool Function::is_time_interval() const {
  /// Check if this is a function on objects
  if (is_object_op()) {
    return false;
  }
  /// If it isn't an object function, we need to check if each argument is a
  /// constant, a time variable, or recursively check if the sub-function is a
  /// time-interval.
  bool ret = true;
  for (const auto& op : this->args) {
    ret = ret && utils::visit(
                     utils::overloaded{
                         [](const Constant&) { return true; },
                         [](const Variable& var) {
                           return var.is_timepoint() || var.is_frame();
                         },
                         [](const Function& fun) { return fun.is_time_interval(); },
                         [](const auto&) { return false; },
                     },
                     op);
  }
  return ret;
}

PinnedFrame::PinnedFrame(ExprPtr time_v, ExprPtr frame_v, ExprPtr subexpr) :
    time_var{std::move(time_v)},
    frame_var{std::move(frame_v)},
    arg{std::move(subexpr)} {
  /// We need to check if
  /// 1. Both, Var_x and Var_t cannot be nullptr
  /// 2. The correct Variable types are used.
  if (time_var == nullptr && frame_var == nullptr) {
    throw std::invalid_argument(
        "At least one variable needs to be defined in the Pin expression");
  }

  if (time_var != nullptr) {
    if (auto var_ptr = std::get_if<Variable>(time_var.get())) {
      if (!var_ptr->is_timepoint()) {
        throw std::invalid_argument("Given 'time_var' does not have type Timepoint");
      }
    } else {
      throw std::invalid_argument("Given 'time_var' is not a Variable");
    }
  }

  if (frame_var != nullptr) {
    if (auto var_ptr = std::get_if<Variable>(frame_var.get())) {
      if (!var_ptr->is_frame()) {
        throw std::invalid_argument("Given 'frame_var' does not have type Frame");
      }
    } else {
      throw std::invalid_argument("Given 'frame_var' is not a Variable");
    }
  }

  // We also need to check if the argument is a valid expression (Temporal Logic one).
  if (arg == nullptr) {
    throw std::invalid_argument("Pinned frame cannot have null sub-expression");
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
      *arg);
  if (!is_valid) {
    throw std::invalid_argument(
        "Pinned frame sub-expression must be a Temporal Logic expression");
  }
}

std::string PinnedFrame::to_string() const {
  const auto time_str  = (time_var == nullptr) ? "_" : time_var->to_string();
  const auto frame_str = (frame_var == nullptr) ? "_" : frame_var->to_string();

  return fmt::format("(@ ({} {}) {})", time_str, frame_str, arg->to_string());
}

Interval::Interval(ExprPtr interval_expr) : interval{std::move(interval_expr)} {
  // Check if
  // 1. The Interval is a Predicate or a Propositonal operation that only contains
  //    timing constraints in it's syntax tree.
  if (interval != nullptr) {
    bool check = utils::visit(
        utils::overloaded{
            [](const Constant&) { return true; },
            [](const PredicateOp& fn) { return fn.is_time_interval(); },
            [](const LogicalOp& fn) { return fn.is_time_interval(); },
            [](const auto&) { return false; },
        },
        *interval);
    if (!check) {
      throw std::invalid_argument(
          "Invalid interval expression. Must consist of Predicates or Propositional operations consisting only of time variables or constants");
    }
  } else {
    throw std::logic_error(
        "Interval shouldn't have a null expression as it doesn't make sense");
  }
}

std::string Interval::to_string() const {
  if (interval == nullptr) {
    return "";
  }
  return fmt::format("(_ {})", interval->to_string());
}

} // namespace ast::details

ExprPtr Expr::Function(ast::FnType op, ExprPtrContainer args, AttrContainer attrs) {
  return make_expr(ast::details::Function{op, std::move(args), std::move(attrs)});
}

ExprPtr Expr::Function(
    std::string op,
    std::vector<ExprPtr> args,
    std::set<ast::Attribute, ast::Attribute::KeyCompare> attrs) {
  auto fntype = magic_enum::enum_cast<ast::FnType>(op);
  if (fntype.has_value()) {
    if (*fntype != ast::FnType::Custom) {
      return Function(*fntype, std::move(args), std::move(attrs));
    }
  }
  return make_expr(percemon::ast::details::Function{
      std::move(op), std::move(args), std::move(attrs)});
}

ExprPtr Expr::Dist(ExprPtr x, ExprPtr y, AttrContainer attrs) {
  return Function(ast::FnType::Dist, {std::move(x), std::move(y)}, std::move(attrs));
}

ExprPtr Expr::Offset(ExprPtr x, AttrContainer attrs) {
  return Function(ast::FnType::Offset, {std::move(x)}, std::move(attrs));
}

ExprPtr Expr::Class(ExprPtr obj) {
  return Function(ast::FnType::Class, {std::move(obj)}, {});
}

ExprPtr Expr::Prob(ExprPtr obj) {
  return Function(ast::FnType::Prob, {std::move(obj)}, {});
}

ExprPtr Expr::Add(std::vector<ExprPtr> args) {
  return Function(ast::FnType::Add, std::move(args), {});
}

ExprPtr Expr::Mul(std::vector<ExprPtr> args) {
  return Function(ast::FnType::Mul, std::move(args), {});
}

ExprPtr Expr::Subtract(ExprPtr lhs, ExprPtr rhs) {
  return Function(ast::FnType::Sub, {std::move(lhs), std::move(rhs)}, {});
}

ExprPtr Expr::Div(ExprPtr num, ExprPtr den) {
  return Function(ast::FnType::Div, {std::move(num), std::move(den)}, {});
}

ExprPtr Expr::PinAt(ExprPtr time_var, ExprPtr frame_var, ExprPtr subexpr) {
  return make_expr(ast::details::PinnedFrame{
      std::move(time_var), std::move(frame_var), std::move(subexpr)});
}

ExprPtr Expr::PinAtFrame(ExprPtr frame_var, ExprPtr subexpr) {
  return PinAt(nullptr, std::move(frame_var), std::move(subexpr));
}

ExprPtr Expr::PinAtTime(ExprPtr time_var, ExprPtr subexpr) {
  return PinAt(std::move(time_var), nullptr, std::move(subexpr));
}

std::shared_ptr<ast::Interval> Expr::Interval(ExprPtr expr) {
  return std::make_shared<ast::Interval>(std::move(expr));
}

} // namespace percemon
