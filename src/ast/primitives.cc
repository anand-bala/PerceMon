#include "percemon/ast/expression.hpp" // for Expr, ExprPtr, Parameter, Variable, Constant
#include "utils/visit.hpp"             // for overloaded, visit

#include <string>  // for string, basic_string
#include <utility> // for move

#include <fmt/format.h> // for format
#include <magic_enum.hpp>

namespace percemon {

namespace ast::details {

std::string Constant::to_string() const {
  return utils::visit(
      utils::overloaded{
          [](const std::string& s) { return fmt::format("\"{}\"", s); },
          [](const C_TIME&) { return fmt::format("C_TIME"); },
          [](const C_FRAME&) { return fmt::format("C_FRAME"); },
          [](auto c) { return fmt::format("{}", c); },
      },
      *this);
}

std::string Variable::to_string() const {
  return name;
}

} // namespace ast::details

ExprPtr Expr::C_TIME() {
  return Constant(ast::details::C_TIME{});
}

ExprPtr Expr::C_FRAME() {
  return Constant(ast::details::C_FRAME{});
}

ExprPtr Expr::Variable(std::string name, std::string type) {
  auto var_type = magic_enum::enum_cast<ast::VarType>(type);
  if (var_type.has_value()) {
    return make_expr(ast::details::Variable{std::move(name), *var_type});
  } else {
    return make_expr(
        ast::details::Variable{std::move(name), ast::VarType::Custom, std::move(type)});
  }
}

ExprPtr Expr::Variable(std::string name, ast::VarType type) {
  return make_expr(ast::details::Variable{std::move(name), type});
}

ExprPtr Expr::Var_f(std::string name) {
  return Variable(std::move(name), ast::VarType::Frame);
}
ExprPtr Expr::Var_t(std::string name) {
  return Variable(std::move(name), ast::VarType::Timepoint);
}

ExprPtr Expr::Var_id(std::string name) {
  return Variable(std::move(name), ast::VarType::Object);
}

} // namespace percemon
