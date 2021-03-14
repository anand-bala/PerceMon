#include "percemon/ast/expression.hpp" // for Expr, ExprPtr, Parameter, Variable, Constant
#include "utils/visit.hpp"             // for overloaded, visit

#include <string>  // for string, basic_string
#include <utility> // for move

#include <fmt/format.h> // for format

using namespace percemon::ast::details;

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
