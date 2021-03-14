#include "percemon/ast/expression.hpp" // for LogicalOp, PredicateOp, Expr, ExprPtr
#include "utils/visit.hpp"             // for overloaded, visit

#include <fmt/format.h>                // for format, join
#include <magic_enum.hpp>              // for enum_name
#include <range/v3/view/enumerate.hpp> // for enumerate, enumerate_fn
#include <range/v3/view/view.hpp>      // for view_closure
#include <range/v3/view/zip.hpp>       // for zip_view

#include <memory>      // for operator==, __shared_ptr_access
#include <stdexcept>   // for invalid_argument
#include <string>      // for string
#include <string_view> // for string_view
#include <utility>     // for move
#include <vector>      // for vector

namespace percemon {

namespace ast::details {

PredicateOp::PredicateOp(Type cmp, ExprPtr arg1, ExprPtr arg2) :
    op{cmp}, lhs{std::move(arg1)}, rhs{std::move(arg2)} {
  // 1. Check if the arguments exist.
  if (lhs == nullptr || rhs == nullptr) {
    throw std::invalid_argument("Predicate has non-existent (nullptr) arguments");
  }
  // 2. Since neither of the arguments are nullptr, we can check their types.
  //
  // A predicate must have as argument either Constants, Variables, or Functions.
  // We also comparison of Area operations.
  // Let us check this.
  constexpr auto if_valid_op = utils::overloaded{
      [](const Constant&) { return true; },
      [](const Variable&) { return true; },
      [](const Function&) { return true; },
      [](const SpatialOp& fn) { return fn.op == SpatialOp::Type::Area; },
      [](const auto&) {
        return false;
      }};
  // First check the LHS
  if (bool lhs_ok = utils::visit(if_valid_op, *lhs); !lhs_ok) {
    {
      throw std::invalid_argument(
          "LHS of Predicate is invalid: not a Constant, Parameter, Variable, Function, or Area operation.");
    }

    // Now check the RHS
    if (bool rhs_ok = utils::visit(if_valid_op, *rhs); !rhs_ok) {
      throw std::invalid_argument(
          "RHS of Predicate is invalid: not a Constant, Parameter, Variable, Function, or Area operation.");
    }
  }
}

std::string PredicateOp::to_string() const {
  std::string_view op_str;
  switch (op) {
    case Type::LE:
      op_str = "<=";
      break;
    case Type::LT:
      op_str = "<";
      break;
    case Type::GE:
      op_str = ">=";
      break;
    case Type::GT:
      op_str = ">";
      break;
    case Type::EQ:
      op_str = "eq";
      break;
    case Type::NE:
      op_str = "neq";
      break;
  }
  return fmt::format("({} {} {})", op, lhs->to_string(), rhs->to_string());
}

bool PredicateOp::is_time_interval() const {
  // For lhs and rhs, check if they contain only time_interval expressions.
  constexpr auto is_ok = utils::overloaded{
      [](const Constant&) { return true; },
      [](const Variable& var) { return var.is_frame() || var.is_timepoint(); },
      [](const Function& fun) { return fun.is_time_interval(); },
      [](const auto&) { return false; },
  };
  bool lhs_ok = utils::visit(is_ok, *lhs);
  bool rhs_ok = utils::visit(is_ok, *rhs);
  return lhs_ok && rhs_ok;
}

LogicalOp::LogicalOp(Type operation, std::vector<ExprPtr> operands) :
    op{operation}, args{std::move(operands)} {
  // Here, we need to check:
  //
  // 1. If the type is Not, then the number of arguments is 1. Otherwise >= 2.
  // 2. Each argument must be a Predicate, a LogicalOp, a TemporalOp, or a
  //    SpatialQuantifier

  // 1. Check nargs
  if (op == Type::Not) {
    if (args.size() != 1) {
      throw std::invalid_argument(fmt::format(
          "Unary `not` operation expects exactly 1 argument, got {}", args.size()));
    }
  } else { // And or Or
    if (args.size() < 2) {
      throw std::invalid_argument(fmt::format(
          "N-ary `{}` operation expects at least 2 arguments, got {}",
          magic_enum::enum_name(op),
          args.size()));
    }
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
          "Argument at position {} is not valid: must be a Predicate, a Logical Operation, or a Temporal Operation",
          idx));
    }
  }
}

std::string LogicalOp::to_string() const {
  std::string_view op_str;
  switch (op) {
    case Type::Not:
      op_str = "not";
      break;
    case Type::And:
      op_str = "and";
      break;
    case Type::Or:
      op_str = "or";
      break;
  }

  auto args_str = std::vector<std::string>(args.size());
  for (const auto& sub_expr : args) { args_str.push_back(sub_expr->to_string()); }
  return fmt::format("({} {})", op, fmt::join(args_str, " "));
}

bool LogicalOp::is_time_interval() const {
  bool ret = true;
  for (const auto& arg : this->args) {
    ret = ret && utils::visit(
                     utils::overloaded{
                         [](const Constant&) { return true; },
                         [](const PredicateOp& e) { return e.is_time_interval(); },
                         [](const LogicalOp& e) { return e.is_time_interval(); },
                         [](const auto&) { return false; },
                     },
                     *arg);
  }
  return ret;
}

QuantifierOp::QuantifierOp(
    Type quantifier,
    std::vector<ExprPtr> variables,
    ExprPtr subexpr) :
    op{quantifier}, vars{std::move(variables)}, arg{std::move(subexpr)} {
  // We should have at least 1 variable.
  if (this->vars.empty()) {
    throw std::invalid_argument("Quantifier must have at least 1 variable");
  }
  /// We need to check if all vars are ID Variables
  bool all_id_vars = true;
  bool any_is_null = false;
  for (const auto& expr : this->vars) {
    if (expr == nullptr) {
      any_is_null = true;
      break;
    } else if (const auto e_ptr = std::get_if<Variable>(expr.get())) {
      all_id_vars = all_id_vars && e_ptr->is_object();
    } else {
      all_id_vars = false;
      break;
    }
  }
  if (any_is_null) {
    throw std::invalid_argument("Some variables are null");
  }
  if (!all_id_vars) {
    throw std::invalid_argument(
        "All variables in the quantifier must be of Object ID variables");
  }
  /// Check if arg is nullptr
  if (this->arg == nullptr) {
    throw std::invalid_argument("Quantifier cannot have null sub expression");
  }

  /// Then, we need to check if the arg is not a Constant, a Function, a SpatialOp, or a
  /// SpatioTemporalOp.
  bool arg_is_ok = utils::visit(
      utils::overloaded{
          [](const Constant&) { return false; },
          [](const Function&) { return false; },
          [](const SpatialOp&) { return false; },
          [](const SpatioTemporalOp&) { return false; },
          [](const auto&) { return true; },
      },
      this->arg);
  if (!arg_is_ok) {
    throw std::invalid_argument(
        "Quantifier expression cannot have Constant, Function, SpatialOp, or SpatioTemporalOp as argument");
  }
}

std::string QuantifierOp::to_string() const {
  auto vars_str = std::vector<std::string>{};
  vars_str.reserve(vars.size());
  for (const auto& var : this->vars) { vars_str.push_back(var->to_string()); }
  return fmt::format(
      "({} ({}) {})",
      magic_enum::enum_name(this->op),
      fmt::join(vars_str, " "),
      arg->to_string());
}

} // namespace ast::details

namespace nodes = ast::details;

ExprPtr Expr::Eq(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{ast::CmpOp::EQ, std::move(lhs), std::move(rhs)});
}

ExprPtr Expr::Neq(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{ast::CmpOp::NE, std::move(lhs), std::move(rhs)});
}

ExprPtr Expr::Lt(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{ast::CmpOp::LT, std::move(lhs), std::move(rhs)});
}

ExprPtr Expr::Le(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{ast::CmpOp::LE, std::move(lhs), std::move(rhs)});
}

ExprPtr Expr::Gt(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{ast::CmpOp::GT, std::move(lhs), std::move(rhs)});
}

ExprPtr Expr::Ge(ExprPtr lhs, ExprPtr rhs) {
  return make_expr(nodes::PredicateOp{ast::CmpOp::GE, std::move(lhs), std::move(rhs)});
}

ExprPtr Expr::Not(ExprPtr arg) {
  return make_expr(nodes::LogicalOp{ast::LogicOpType::Not, {std::move(arg)}});
}

ExprPtr Expr::And(std::vector<ExprPtr> arg) {
  return make_expr(nodes::LogicalOp{ast::LogicOpType::And, std::move(arg)});
}

ExprPtr Expr::Or(std::vector<ExprPtr> arg) {
  return make_expr(nodes::LogicalOp{ast::LogicOpType::Or, std::move(arg)});
}

ExprPtr Expr::Implies(ExprPtr x, ExprPtr y) {
  ExprPtr not_x = Not(std::move(x));
  return Or({std::move(not_x), std::move(y)});
}

ExprPtr Expr::Xor(ExprPtr x, ExprPtr y) {
  ExprPtr not_x    = Not(x);
  ExprPtr not_y    = Not(y);
  ExprPtr x_or_y   = Or({std::move(x), std::move(y)});
  ExprPtr nx_or_ny = Or({std::move(not_x), std::move(not_y)});
  return And({std::move(x_or_y), std::move(nx_or_ny)});
}

ExprPtr Expr::Iff(ExprPtr x, ExprPtr y) {
  ExprPtr not_x     = Not(x);
  ExprPtr not_y     = Not(y);
  ExprPtr x_and_y   = And({std::move(x), std::move(y)});
  ExprPtr nx_and_ny = And({std::move(not_x), std::move(not_y)});
  return Or({std::move(x_and_y), std::move(nx_and_ny)});
}

ExprPtr Expr::Exists(ExprPtrContainer vars, ExprPtr arg) {
  return make_expr(nodes::QuantifierOp{
      ast::QuantifierType::Exists, std::move(vars), std::move(arg)});
}

ExprPtr Expr::Forall(ExprPtrContainer vars, ExprPtr arg) {
  return make_expr(nodes::QuantifierOp{
      ast::QuantifierType::Forall, std::move(vars), std::move(arg)});
}

} // namespace percemon
