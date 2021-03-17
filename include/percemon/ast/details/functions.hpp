/// @file     ast/details/functions.hpp
/// @brief    Generic function AST definitions.
///
/// Here, we define a generic AST node for functions, which can have arbitrary number of
/// arguments and attributes that change the semantics of the function. In the context
/// of STQL, involves functions on the following data types: Constants, Variables,  and
/// other Functions. Functions do not involve the usual Logical operators, as they have
/// special meaning in our semantics, nor do they include Predicates defined by
/// relational operations (as they are the smallest non-Constant boolean expressions),
/// but they do include arithmetic operations (`+, -, /, *`), and any other mathematical
/// operations supported by the semantics.
///
/// We also define the AST node for pinning operations `(@ x f)` and interval operations
/// `(_ term)`.

#pragma once
#ifndef PERCEMON_AST_DETAILS_FUNCTIONS
#define PERCEMON_AST_DETAILS_FUNCTIONS

#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "percemon/ast/ast_fwd.hpp"
#include "percemon/ast/details/attributes.hpp"

namespace percemon::ast::details {

/// @brief Functions on `Constant`s, `Variable`s, and other `Function`s.
///
/// This can refer to mathematical functions (excluding logical operations).
struct ArithmeticFn {
  enum struct Type { Add, Sub, Mul, Div, Dist, Offset, Class, Prob, Custom };

  Type fn;
  std::optional<std::string> custom_fn;
  std::vector<ExprPtr> args;
  std::set<Attribute, Attribute::KeyCompare> attrs;

  ArithmeticFn(
      Type op,
      std::optional<std::string> op_str,
      std::vector<ExprPtr> operands,
      std::set<Attribute, Attribute::KeyCompare> attributes);

  ArithmeticFn(Type op, std::vector<ExprPtr> operands) :
      ArithmeticFn{op, std::nullopt, std::move(operands), {}} {}

  ArithmeticFn(
      Type op,
      std::vector<ExprPtr> operands,
      std::set<Attribute, Attribute::KeyCompare> attributes) :
      ArithmeticFn{op, std::nullopt, std::move(operands), std::move(attributes)} {}

  ArithmeticFn(
      std::string op,
      std::vector<ExprPtr> operands,
      std::set<Attribute, Attribute::KeyCompare> attributes) :
      ArithmeticFn{
          Type::Custom,
          std::move(op),
          std::move(operands),
          std::move(attributes)} {}

  [[nodiscard]] bool is_arithmetic() const {
    switch (fn) {
      case Type::Add:
      case Type::Mul:
      case Type::Sub:
      case Type::Div:
        return true;
      default:
        return false;
    }
  }

  [[nodiscard]] bool is_object_op() const {
    switch (fn) {
      case Type::Class:
      case Type::Prob:
      case Type::Offset:
      case Type::Dist:
        return true;
      default:
        return false;
    }
  }

  [[nodiscard]] bool is_custom() const {
    return fn == Type::Custom;
  }

  [[nodiscard]] std::string to_string() const;

  void validate() const;
};

/// @brief Pinned time and frame variables for some sub-expression.
struct PinnedFrame {
  ExprPtr time_var  = nullptr;
  ExprPtr frame_var = nullptr;

  ExprPtr arg;

  PinnedFrame(ExprPtr time_v, ExprPtr frame_v, ExprPtr subexpr);

  [[nodiscard]] std::string to_string() const;
};

/// @brief Interval constraint holder.
///
/// An interval is essentially some boolean function on functions, constants, and
/// primitives.
struct IntervalConstraint {
  ExprPtr interval = nullptr;

  IntervalConstraint(ExprPtr interval_expr);

  [[nodiscard]] std::string to_string() const;
};

/// @brief Interval type.
///
/// @note
/// An interval can only hold Constants or Parameters for the lower and upper bounds.
/// This is asserted at construction time.
struct Interval {
  enum struct Type { Frame, Time };

  Type type;
  ExprPtr low;
  ExprPtr high;

  Interval() = default;

  Interval(Type, ExprPtr, ExprPtr);

  [[nodiscard]] std::string to_string() const;
};

} // namespace percemon::ast::details

#endif /* end of include guard: PERCEMON_AST_DETAILS_FUNCTIONS */
