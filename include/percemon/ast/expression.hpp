/// @file     ast/expression.hpp
/// @brief    Interface to create valid expressions.

#pragma once
#ifndef PERCEMON_AST_EXPRESSION
#define PERCEMON_AST_EXPRESSION

#include "percemon/ast/ast_fwd.hpp"

// IWYU pragma: begin_exports
#include "percemon/ast/details/functions.hpp"
#include "percemon/ast/details/primitives.hpp"
#include "percemon/ast/details/propositional.hpp"
#include "percemon/ast/details/spatial.hpp"
#include "percemon/ast/details/temporal.hpp"
// IWYU pragma: end_exports

#include <cstddef>   // for size_t
#include <memory>    // for shared_ptr
#include <set>       // for set
#include <stdexcept> // for invalid_argument
#include <string>    // for string
#include <utility>   // for move
#include <variant>   // for variant
#include <vector>    // for vector

namespace percemon {
namespace ast {

using VarType               = details::Variable::Type;
using FnType                = details::ArithmeticFn::Type;
using CmpOp                 = details::PredicateOp::Type;
using LogicOpType           = details::LogicalOp::Type;
using QuantifierType        = details::QuantifierOp::Type;
using ModalOpType           = details::TemporalOp::Type;
using SpatialOpType         = details::SpatialOp::Type;
using SpatialQuantifierType = details::SpatialQuantifier::Type;
using SpModalOpType         = details::SpatioTemporalOp::Type;

using details::Attribute;
using details::Interval;
using IntervalPtr = std::shared_ptr<Interval>;

namespace details {
using ExprVariant = std::variant<
    Constant,
    Variable,
    ArithmeticFn,
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
using AttrContainer    = std::set<ast::Attribute, ast::Attribute::KeyCompare>;

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

  /// The Current Time primitive
  static ExprPtr C_TIME();
  /// The Current Frame primitive
  static ExprPtr C_FRAME();
  /// @brief Create an expression with a Constant value.
  ///
  /// @see percemon::ast::details::Constant
  template <typename ConstType>
  static ExprPtr Constant(ConstType constant) {
    return make_expr(ast::details::Constant{std::move(constant)});
  }

  /// @brief Create a Frame variable with the given name
  static ExprPtr Var_f(std::string name);
  /// @brief Create a Timepoint variable with the given name
  static ExprPtr Var_t(std::string name);
  /// @brief Create an Object ID variable
  static ExprPtr Var_id(std::string name);

  /// @brief Create a variable with a string type.
  static ExprPtr Variable(std::string name, std::string type);
  /// @brief Create a variable with a known type.
  static ExprPtr Variable(std::string name, ast::VarType type);

  /// @brief Create a pre-defined function, with given arguments and attributes
  static ExprPtr
  ArithmeticFn(ast::FnType op, ExprPtrContainer args, AttrContainer attrs);

  /// @brief Create a custom function, with given arguments and attributes
  static ExprPtr Function(std::string op, ExprPtrContainer args, AttrContainer attrs);

  /// @brief Distance between two topological surfaces
  static ExprPtr Dist(ExprPtr x, ExprPtr y, AttrContainer attrs);

  /// @brief Offset of a topological structure
  static ExprPtr Offset(ExprPtr arg, AttrContainer attrs);

  /// @brief Class of an object
  static ExprPtr Class(ExprPtr obj);

  /// @brief Confidence of an object detection
  static ExprPtr Prob(ExprPtr obj);

  /// @brief Create an Addition AST
  static ExprPtr Add(ExprPtrContainer args);

  /// @brief Create an Multiplication AST
  static ExprPtr Mul(ExprPtrContainer args);

  /// @brief Create an Subtraction AST
  static ExprPtr Sub(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create an Division AST
  static ExprPtr Div(ExprPtr numerator, ExprPtr denominator);

  /// @brief Create a Equality predicate
  static ExprPtr Eq(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Not-Equality predicate
  static ExprPtr Neq(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Less Than predicate
  static ExprPtr Lt(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Less Than or Equal predicate
  static ExprPtr Le(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Greater Than predicate
  static ExprPtr Gt(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Greater Than or Equal predicate
  static ExprPtr Ge(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a pinned frame and time variable for some subexpression.
  static ExprPtr PinAt(ExprPtr time_var, ExprPtr frame_var, ExprPtr subexpr);

  /// @brief Create a pinned frame.
  static ExprPtr PinAtFrame(ExprPtr frame_var, ExprPtr subexpr);

  /// @brief Create a pinned frame.
  static ExprPtr PinAtTime(ExprPtr time_var, ExprPtr subexpr);

  /// @brief Create an Interval expression
  // static ast::IntervalPtr Interval(ExprPtr expr);

  static ast::IntervalPtr FrameInterval(ExprPtr low, ExprPtr high);
  static ast::IntervalPtr TimeInterval(ExprPtr low, ExprPtr high);

  /// @brief Create a Logical Negation
  static ExprPtr Not(ExprPtr arg);

  /// @brief Create a Logical And operation
  static ExprPtr And(ExprPtrContainer arg);

  /// @brief Create a Logical Or operation
  static ExprPtr Or(ExprPtrContainer arg);

  /// @brief Create a Logical Implication
  static ExprPtr Implies(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Logical Implication
  static ExprPtr Xor(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create a Logical Implication
  static ExprPtr Iff(ExprPtr lhs, ExprPtr rhs);

  /// @brief Create an Existential quantifier
  static ExprPtr Exists(ExprPtrContainer vars, ExprPtr arg);

  /// @brief Create an Universal quantifier
  static ExprPtr Forall(ExprPtrContainer vars, ExprPtr arg);

  /// @brief Next temporal operator
  static ExprPtr Next(ExprPtr arg);

  /// @brief Previous temporal operator
  static ExprPtr Previous(ExprPtr arg);

  /// @brief Eventually temporal operator
  static ExprPtr Eventually(ExprPtr arg);

  /// @brief Eventually temporal operator
  static ExprPtr Eventually(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Once temporal operator
  static ExprPtr Once(ExprPtr arg);

  /// @brief Once temporal operator
  static ExprPtr Once(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Always temporal operator
  static ExprPtr Always(ExprPtr arg);

  /// @brief Always temporal operator
  static ExprPtr Always(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Historically temporal operator
  static ExprPtr Historically(ExprPtr arg);

  /// @brief Historically temporal operator
  static ExprPtr Historically(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Until temporal operator
  static ExprPtr Until(ExprPtr arg1, ExprPtr arg2);

  /// @brief Until temporal operator
  static ExprPtr
  Until(ExprPtr arg1, ExprPtr arg2, std::shared_ptr<ast::Interval> interval);

  /// @brief Since temporal operator
  static ExprPtr Since(ExprPtr arg1, ExprPtr arg2);

  /// @brief Since temporal operator
  static ExprPtr
  Since(ExprPtr arg1, ExprPtr arg2, std::shared_ptr<ast::Interval> interval);

  // SpatialOp,
  /// @brief Bounding Box associated with an object
  static ExprPtr BBox(ExprPtr obj);

  /// @brief Area of a topological structure
  static ExprPtr Area(ExprPtr obj);

  /// @brief Complement a topological object
  static ExprPtr Complement(ExprPtr arg);

  /// @brief Create a spatial union operation
  static ExprPtr Union(ExprPtrContainer arg);

  /// @brief Create a spatial intersection operation
  static ExprPtr Intersect(ExprPtrContainer arg);

  /// @brief Create a spatial extential quantifier
  static ExprPtr NotEmpty(ExprPtr arg);

  /// @brief Create a spatial universal quantifier
  static ExprPtr Fills(ExprPtr arg);

  /// @brief Spatio-temporal Next operator
  static ExprPtr SpatialNext(ExprPtr arg);

  /// @brief Previous spatio-temporal operator
  static ExprPtr SpatialPrevious(ExprPtr arg);

  /// @brief Eventually spatio-temporal operator
  static ExprPtr SpatialEventually(ExprPtr arg);

  /// @brief Eventually spatio-temporal operator
  static ExprPtr
  SpatialEventually(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Once spatio-temporal operator
  static ExprPtr SpatialOnce(ExprPtr arg);

  /// @brief Once spatio-temporal operator
  static ExprPtr SpatialOnce(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Always spatio-temporal operator
  static ExprPtr SpatialAlways(ExprPtr arg);

  /// @brief Always spatio-temporal operator
  static ExprPtr SpatialAlways(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Historically spatio-temporal operator
  static ExprPtr SpatialHistorically(ExprPtr arg);

  /// @brief Historically spatio-temporal operator
  static ExprPtr
  SpatialHistorically(ExprPtr arg, std::shared_ptr<ast::Interval> interval);

  /// @brief Until spatio-temporal operator
  static ExprPtr SpatialUntil(ExprPtr arg1, ExprPtr arg2);

  /// @brief Until spatio-temporal operator
  static ExprPtr
  SpatialUntil(ExprPtr arg1, ExprPtr arg2, std::shared_ptr<ast::Interval> interval);

  /// @brief Since spatio-temporal operator
  static ExprPtr SpatialSince(ExprPtr arg1, ExprPtr arg2);

  /// @brief Since spatio-temporal operator
  static ExprPtr
  SpatialSince(ExprPtr arg1, ExprPtr arg2, std::shared_ptr<ast::Interval> interval);

  [[nodiscard]] size_t id() const {
    return this->m_id;
  }

  [[nodiscard]] std::string to_string() const;

 private:
  /// @brief The unique ID for an expression
  ///
  /// The ID of the Expr is used to create look-up tables within contexts, allowing for
  /// efficient use of the syntax tree.
  ///
  /// We set the initial value explicitely to 0 so that we can detect if the hash was
  /// set by us (as the probability of a hash being 0 is incredibly low due to number
  /// theory magic).
  size_t m_id = 0;

  /// Private factory function.
  static ExprPtr make_expr(Expr&&);
};

using ExprPtr = ExprPtr;
} // namespace percemon

#endif /* end of include guard: PERCEMON_AST_EXPRESSION */
