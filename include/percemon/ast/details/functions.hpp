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
/// We also define the AST node for pinning operations `(@ x f)`.

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

namespace PERCEMON_AST_NS {

/// @brief Functions on `Constant`s, `Variable`s, and other `Function`s.
///
/// This can refer to mathematical functions (excluding logical operations) or to STQL
/// specific functions like Euclidean distance, Class reference, Probability reference,
/// etc.
struct Function {
  enum struct Type {
    Add,
    Sub,
    Mul,
    Div,
    Dist,
    Offset,
    Class,
    Prob,
    Area,
    BBox,
    Custom
  };

  Type fn;
  std::vector<ExprPtr> args;
  std::optional<std::string> custom_fn;

  std::set<std::string> attrs;

  Function(Type op, std::vector<ExprPtr> operands, std::set<std::string> attributes) :
      fn{op}, args{std::move(operands)}, attrs{std::move(attributes)} {}

  Function(
      std::string op,
      std::vector<ExprPtr> operands,
      std::set<std::string> attributes) :
      fn{Type::Custom},
      args{std::move(operands)},
      custom_fn{std::move(op)},
      attrs{std::move(attributes)} {}
};

/// @brief Pinned time and frame variables
struct PinnedFrame {
  ExprPtr time_var  = nullptr;
  ExprPtr frame_var = nullptr;
};

/// @brief Interval constraint holder.
///
/// An interval is essentially some boolean function on functions, constants, and
/// primitives.
struct Interval {
  ExprPtr interval = nullptr;
};

} // namespace PERCEMON_AST_NS

#endif /* end of include guard: PERCEMON_AST_DETAILS_FUNCTIONS */
