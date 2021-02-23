/// @file     ast/expression.hpp
/// @brief    Interface to create valid expressions.

#pragma once
#ifndef PERCEMON_AST_EXPRESSION
#define PERCEMON_AST_EXPRESSION

#include <initializer_list>
#include <memory>
#include <variant>

#include "percemon/ast/ast_fwd.hpp"

// IWYU pragma: begin_exports
#include "percemon/ast/details/functions.hpp"
#include "percemon/ast/details/primitives.hpp"
#include "percemon/ast/details/propositional.hpp"
#include "percemon/ast/details/spatial.hpp"
#include "percemon/ast/details/temporal.hpp"
// IWYU pragma: end_exports

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
    PinnedFrame,
    TemporalOp,
    SpatialOp,
    SpatialQuantifier,
    SpatioTemporalOp>;
} // namespace details
} // namespace ast

using ExprPtrContainer = std::vector<ExprPtr>;
using AttrContainer    = std::set<std::string>;

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
  /// @brief Create a variable with a string type.
  static std::unique_ptr<Expr> Variable(std::string name, std::string type);
  /// @brief Create a variable with a known type.
  static std::unique_ptr<Expr> Variable(std::string name, ast::VarType type);

  /// @brief Create a pre-defined function, with given arguments and attributes
  static std::unique_ptr<Expr>
  Function(ast::FnType op, ExprPtrContainer args, AttrContainer attrs);

  /// @brief Create a custom function, with given arguments and attributes
  static std::unique_ptr<Expr>
  Function(std::string op, ExprPtrContainer args, AttrContainer attrs);

  /// @brief Create a Equality predicate
  static std::unique_ptr<Expr> Eq(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Not-Equality predicate
  static std::unique_ptr<Expr> Neq(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Less Than predicate
  static std::unique_ptr<Expr> Lt(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Less Than or Equal predicate
  static std::unique_ptr<Expr> Le(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Greater Than predicate
  static std::unique_ptr<Expr> Gt(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Greater Than or Equal predicate
  static std::unique_ptr<Expr> Ge(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Logical Negation
  static std::unique_ptr<Expr> Not(ExprPtr arg);

  /// @brief Create a Logical And operation
  static std::unique_ptr<Expr> And(ExprPtrContainer arg);

  /// @brief Create a Logical Or operation
  static std::unique_ptr<Expr> Or(ExprPtrContainer arg);

  /// @brief Create a Logical Implication
  static std::unique_ptr<Expr> Implies(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Logical Implication
  static std::unique_ptr<Expr> Xor(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Logical Implication
  static std::unique_ptr<Expr> Iff(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create an Existential quantifier
  static std::unique_ptr<Expr> Exists(ExprPtrContainer vars, ExprPtr arg);

  /// @brief Create an Universal quantifier
  static std::unique_ptr<Expr> Forall(ExprPtrContainer vars, ExprPtr arg);

  /// @brief Next temporal operator
  static std::unique_ptr<Expr> Next(ExprPtr arg);

  /// @brief Previous temporal operator
  static std::unique_ptr<Expr> Previous(ExprPtr arg);

  /// @brief Eventually temporal operator
  static std::unique_ptr<Expr> Eventually(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Once temporal operator
  static std::unique_ptr<Expr> Once(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Always temporal operator
  static std::unique_ptr<Expr> Always(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Historically temporal operator
  static std::unique_ptr<Expr> Historically(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Until temporal operator
  static std::unique_ptr<Expr>
  Until(ExprPtr arg1, ExprPtr arg2, ExprPtr interval = nullptr);

  /// @brief Since temporal operator
  static std::unique_ptr<Expr>
  Since(ExprPtr arg1, ExprPtr arg2, ExprPtr interval = nullptr);

  // SpatialOp,
  /// @brief Complement a topological object
  static std::unique_ptr<Expr> Complement(ExprPtr arg);

  /// @brief Create a spatial union operation
  static std::unique_ptr<Expr> Union(ExprPtrContainer arg);

  /// @brief Create a spatial intersection operation
  static std::unique_ptr<Expr> Intersect(ExprPtrContainer arg);

  /// @brief Create a spatial extential quantifier
  static std::unique_ptr<Expr> NotEmpty(ExprPtr arg);

  /// @brief Create a spatial universal quantifier
  static std::unique_ptr<Expr> Fills(ExprPtr arg);

  /// @brief Spatio-temporal Next operator
  static std::unique_ptr<Expr> SpatialNext(ExprPtr arg);

  /// @brief Previous spatio-temporal operator
  static std::unique_ptr<Expr> SpatialPrevious(ExprPtr arg);

  /// @brief Eventually spatio-temporal operator
  static std::unique_ptr<Expr>
  SpatialEventually(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Once spatio-temporal operator
  static std::unique_ptr<Expr> SpatialOnce(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Always spatio-temporal operator
  static std::unique_ptr<Expr> SpatialAlways(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Historically spatio-temporal operator
  static std::unique_ptr<Expr>
  SpatialHistorically(ExprPtr arg, ExprPtr interval = nullptr);

  /// @brief Until spatio-temporal operator
  static std::unique_ptr<Expr>
  SpatialUntil(ExprPtr arg1, ExprPtr arg2, ExprPtr interval = nullptr);

  /// @brief Since spatio-temporal operator
  static std::unique_ptr<Expr>
  SpatialSince(ExprPtr arg1, ExprPtr arg2, ExprPtr interval = nullptr);

  /// @brief Create a pinned frame and time variable.
  static std::unique_ptr<Expr> PinAt(ExprPtr time_var, ExprPtr frame_var);

  /// @brief Create a pinned frame.
  static std::unique_ptr<Expr> PinAtFrame(ExprPtr frame_var);

  /// @brief Create a pinned frame.
  static std::unique_ptr<Expr> PinAtTime(ExprPtr time_var);

  /// @brief Distance between two topological surfaces
  static std::unique_ptr<Expr> Dist(ExprPtr x, ExprPtr y, AttrContainer attrs);

  /// @brief Offset of a topological structure
  static std::unique_ptr<Expr> Offset(ExprPtr arg, AttrContainer attrs);

  /// @brief Class of an object
  static std::unique_ptr<Expr> Class(ExprPtr obj);

  /// @brief Confidence of an object detection
  static std::unique_ptr<Expr> Prob(ExprPtr obj);

  /// @brief Area of a topological structure
  static std::unique_ptr<Expr> Area(ExprPtr obj);

  /// @brief Bounding Box associated with an object
  static std::unique_ptr<Expr> BBox(ExprPtr obj);

  /// @brief Create an Addition AST
  static std::unique_ptr<Expr> Add(ExprPtrContainer args);

  /// @brief Create an Multiplication AST
  static std::unique_ptr<Expr> Mul(ExprPtrContainer args);

  /// @brief Create an Subtraction AST
  static std::unique_ptr<Expr> Subtract(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create an Division AST
  static std::unique_ptr<Expr> Div(ExprPtr numerator, ExprPtr denominator);

  /// @brief Check if the expression is a valid STQL formula
  [[nodiscard]] bool is_valid() const;

 private:
  /// @brief The unique ID for an expression
  ///
  /// The ID of the Expr is used to create look-up tables within contexts, allowing for
  /// efficient use of the
  size_t id;

  /// Private factory function.
  template <typename ExprType>
  static std::unique_ptr<Expr> make_expr(ExprType);
};

using ExprPtr = ExprPtr;
} // namespace percemon

#endif /* end of include guard: PERCEMON_AST_EXPRESSION */
