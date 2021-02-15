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

// Forward-declare Expr
namespace percemon {
struct Expr;
} // namespace percemon

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
  std::shared_ptr<Expr> interval;
  std::array<std::shared_ptr<Expr>, 2> args; // Has max 2 arguments.
};
} // namespace percemon::ast::details

#endif /* end of include guard: PERCEMON_AST_DETAILS_TEMPORAL */
