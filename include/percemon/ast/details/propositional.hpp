/// @file     ast/details/propositional.hpp
/// @brief    Specialized AST nodes for propositional logic.
///
/// Here, we will define the specialized AST nodes for propositional logic operations,
/// including universal and existential quantifiers. Here, we will also define
/// Predicates (using relational operations) as they are the first non-constant boolean
/// expressions.

#pragma once
#ifndef PERCEMON_AST_DETAILS_PROPOSITIONAL
#define PERCEMON_AST_DETAILS_PROPOSITIONAL

#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "percemon/ast/ast_fwd.hpp"

namespace percemon::ast::details {

/// @brief AST node for relational operations/predicates
///
/// @note
/// Predicates can have at most 2 arguments: the LHS and the RHS. Each of these must be
/// either a Constant, a Variable, or a Function.
struct PredicateOp {
  enum struct Type { LE, LT, GE, GT, EQ, NE };

  Type op;
  ExprPtr lhs, rhs;

  PredicateOp(Type cmp, ExprPtr arg1, ExprPtr arg2);

  [[nodiscard]] std::string to_string() const;

  [[nodiscard]] bool is_time_interval() const;
};

/// @brief Generic AST node for all propositional operations.
///
/// @note
/// The argument to a LogicalOp must be either a Predicate, a TemporalOp, another
/// LogicalOp, or a SpatialQuantifier.
struct LogicalOp {
  enum struct Type { Not, And, Or };

  Type op;
  std::vector<ExprPtr> args;

  LogicalOp(Type operation, std::vector<ExprPtr> operands);

  [[nodiscard]] std::string to_string() const;

  [[nodiscard]] bool is_time_interval() const;
};

/// @brief Quantifier expressions
///
/// @note
/// The list of vars must be of type Variable. Also, the arg can't be a Constant,
/// Function, SpatialOp, or SpatioTemporalOp
struct QuantifierOp {
  enum struct Type { Exists, Forall };

  Type op;
  std::vector<ExprPtr> vars;
  ExprPtr arg;

  QuantifierOp(Type quantifier, std::vector<ExprPtr> variables, ExprPtr subexpr);

  [[nodiscard]] std::string to_string() const;
};

} // namespace percemon::ast::details

#endif /* end of include guard: PERCEMON_AST_DETAILS_PROPOSITIONAL */
