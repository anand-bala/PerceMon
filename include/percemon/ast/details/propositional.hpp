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

namespace PERCEMON_AST_NS {

/// @brief AST node for relational operations/predicates
///
/// @note
/// Predicates can have at most 2 arguments: the LHS and the RHS. Each of these must be
/// either a Constant, a Variable, or a Function. This is checked by the semantics.
struct PredicateOp {
  enum struct Type { LE, LT, GE, GT, EQ, NE };

  Type op;
  ExprPtr lhs, rhs;
};

/// @brief Generic AST node for all propositional operations.
struct LogicalOp {
  enum struct Type { Not, And, Or };

  Type op;
  std::vector<ExprPtr> args;
};

/// @brief Quantifier expressions
struct QuantifierOp {
  enum struct Type { Exists, Forall };

  Type op;
  std::vector<ExprPtr> vars;
  ExprPtr arg;
};

} // namespace PERCEMON_AST_NS

#endif /* end of include guard: PERCEMON_AST_DETAILS_PROPOSITIONAL */
