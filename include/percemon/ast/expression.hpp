/// @file     ast/expression.hpp
/// @brief    Interface to create valid expressions.

#pragma once
#ifndef PERCEMON_AST_EXPRESSION
#define PERCEMON_AST_EXPRESSION

#include <initializer_list>
#include <memory>
#include <variant>

// IWYU pragma: begin_exports
#include "percemon/ast/details/functions.hpp"
#include "percemon/ast/details/primitives.hpp"
#include "percemon/ast/details/propositional.hpp"
#include "percemon/ast/details/spatial.hpp"
#include "percemon/ast/details/temporal.hpp"
// IWYU pragma: end_exports

#define PERCEMON_AST_NS percemon::ast::details

namespace percemon {
namespace ast {

using VarType = PERCEMON_AST_NS::Variable::Type;
using FnType  = PERCEMON_AST_NS::Function::Type;

namespace details {
using ExprVariant = std::variant<
    Constant,
    Variable,
    Function,
    PredicateOp,
    LogicalOp,
    QuantifierOp,
    SpatialOp,
    SpatialQuantifier,
    SpatioTemporalOp>;
} // namespace details
} // namespace ast

/// @brief The overarching expression type.
///
/// An expression must be created using the static factory methods given in this struct.
/// This allows for the library to assign the Expr with IDs that allow for efficient
/// look-up table implementations.
///
/// @note
/// We chose to not use a manager class that takes care of this as we can implement this
/// functionality using static thread-safe atomics. But we may change this in future
/// implementations of the library if users run into any issues.
struct Expr : ast::details::ExprVariant {
  using ast::details::ExprVariant::variant;

  /// @brief Create an expression with a Constant value.
  ///
  /// @see percemon::ast::details::Constant
  template <typename CType>
  static std::unique_ptr<Expr> Constant(CType);

  /// @brief Create a Frame variable with the given name
  static std::unique_ptr<Expr> Var_f(std::string name);
  /// @brief Create a Timepoint variable with the given name
  static std::unique_ptr<Expr> Var_t(std::string name);
  /// @brief Create a variable of the given primitive type (`double`, `int` or`bool`)
  template <typename T>
  static std::unique_ptr<Expr> Variable(std::string name);
  /// @brief Create a variable with a custom type.
  template <typename T>
  static std::unique_ptr<Expr> Variable(std::string name, std::string type);

  /// @brief Create a pre-defined function, with given arguments and attributes
  template <typename ExprPtrContainer, typename AttrContainer>
  static std::unique_ptr<Expr>
  Function(ast::FnType op, ExprPtrContainer args, AttrContainer attrs);

  /// @brief Create a custom function, with given arguments and attributes
  template <typename ExprPtrContainer, typename AttrContainer>
  static std::unique_ptr<Expr>
  Function(std::string op, ExprPtrContainer args, AttrContainer attrs);

  /// @brief Create a Equality predicate
  static std::unique_ptr<Expr> Eq(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create a Less Than predicate
  static std::unique_ptr<Expr> Lt(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create a Less Than or Equal predicate
  static std::unique_ptr<Expr> Le(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create a Greater Than predicate
  static std::unique_ptr<Expr> Gt(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create a Greater Than or Equal predicate
  static std::unique_ptr<Expr> Ge(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create a Logical Negation
  static std::unique_ptr<Expr> Not(std::shared_ptr<Expr> arg);

  /// @brief Create a Logical And operation
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr> And(ExprPtrContainer arg);

  /// @brief Create a Logical Or operation
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr> Or(ExprPtrContainer arg);

  /// @brief Create a Logical Implication
  static std::unique_ptr<Expr>
  Implies(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create a Logical Implication
  static std::unique_ptr<Expr>
  Xor(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create a Logical Implication
  static std::unique_ptr<Expr>
  Iff(std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs);

  /// @brief Create an Existential quantifier
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr> Exists(ExprPtrContainer vars, std::shared_ptr<Expr> arg);

  /// @brief Create an Universal quantifier
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr> Forall(ExprPtrContainer vars, std::shared_ptr<Expr> arg);

  /// @brief Next temporal operator
  static std::unique_ptr<Expr> Next(std::shared_ptr<Expr> arg);

  /// @brief Previous temporal operator
  static std::unique_ptr<Expr> Previous(std::shared_ptr<Expr> arg);

  /// @brief Eventually temporal operator
  static std::unique_ptr<Expr>
  Eventually(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Once temporal operator
  static std::unique_ptr<Expr>
  Once(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Always temporal operator
  static std::unique_ptr<Expr>
  Always(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Historically temporal operator
  static std::unique_ptr<Expr>
  Historically(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Until temporal operator
  static std::unique_ptr<Expr> Until(
      std::shared_ptr<Expr> arg1,
      std::shared_ptr<Expr> arg2,
      std::shared_ptr<Expr> interval = nullptr);

  /// @brief Since temporal operator
  static std::unique_ptr<Expr> Since(
      std::shared_ptr<Expr> arg1,
      std::shared_ptr<Expr> arg2,
      std::shared_ptr<Expr> interval = nullptr);

  // SpatialOp,
  /// @brief Complement a topological object
  static std::unique_ptr<Expr> Complement(std::shared_ptr<Expr> arg);

  /// @brief Create a spatial union operation
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr> Union(ExprPtrContainer arg);

  /// @brief Create a spatial intersection operation
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr> Intersect(ExprPtrContainer arg);

  /// @brief Create a spatial extential quantifier
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr>
  NotEmpty(ExprPtrContainer vars, std::shared_ptr<Expr> arg);

  /// @brief Create a spatial universal quantifier
  template <typename ExprPtrContainer>
  static std::unique_ptr<Expr> Fills(ExprPtrContainer vars, std::shared_ptr<Expr> arg);

  /// @brief Next temporal operator
  static std::unique_ptr<Expr> Next(std::shared_ptr<Expr> arg);

  /// @brief Previous temporal operator
  static std::unique_ptr<Expr> Previous(std::shared_ptr<Expr> arg);

  /// @brief Eventually temporal operator
  static std::unique_ptr<Expr>
  Eventually(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Once temporal operator
  static std::unique_ptr<Expr>
  Once(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Always temporal operator
  static std::unique_ptr<Expr>
  Always(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Historically temporal operator
  static std::unique_ptr<Expr>
  Historically(std::shared_ptr<Expr> arg, std::shared_ptr<Expr> interval = nullptr);

  /// @brief Until temporal operator
  static std::unique_ptr<Expr> Until(
      std::shared_ptr<Expr> arg1,
      std::shared_ptr<Expr> arg2,
      std::shared_ptr<Expr> interval = nullptr);

  /// @brief Since temporal operator
  static std::unique_ptr<Expr> Since(
      std::shared_ptr<Expr> arg1,
      std::shared_ptr<Expr> arg2,
      std::shared_ptr<Expr> interval = nullptr);

 private:
  /// @brief The unique ID for an expression
  ///
  /// The ID of the Expr is used to create look-up tables within contexts, allowing for
  /// efficient use of the
  size_t id;
};

using ExprPtr = std::shared_ptr<Expr>;
} // namespace percemon

#endif /* end of include guard: PERCEMON_AST_EXPRESSION */
