/// @file     ast/details/temporal.hpp
/// @brief    AST nodes for temporal operators.
///
/// Here, we define the AST nodes for temporal operators.
#pragma once
#ifndef PERCEMON_AST_DETAILS_TEMPORAL
#define PERCEMON_AST_DETAILS_TEMPORAL

#include <array>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "percemon/ast/ast_fwd.hpp"

namespace percemon::ast::details {
/// @brief Generic AST node for temporal operators
struct TemporalOp {
  enum struct Type {
    Next,
    Previous,
    Eventually,
    Once,
    Always,
    Historically,
    Until,
    Since,
  };

  Type op;
  std::vector<ExprPtr> args; // Has max 2 arguments.
  std::shared_ptr<Interval> interval;

  TemporalOp(
      Type operation,
      std::vector<ExprPtr> arguments,
      std::shared_ptr<Interval> interval_arg);

  TemporalOp(Type operation, std::vector<ExprPtr> arguments) :
      TemporalOp{operation, std::move(arguments), {}} {}

  [[nodiscard]] std::string to_string() const;
};
} // namespace percemon::ast::details

#endif /* end of include guard: PERCEMON_AST_DETAILS_TEMPORAL */
