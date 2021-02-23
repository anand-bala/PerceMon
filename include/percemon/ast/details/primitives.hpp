/// @file     ast/details/primitives.hpp
/// @brief    Primitive types in STQL

#pragma once

#ifndef __PERCEMON_AST_DETAILS_PRIMITIVES_HPP__
#define __PERCEMON_AST_DETAILS_PRIMITIVES_HPP__

#include <optional>
#include <string>
#include <variant>

namespace PERCEMON_AST_NS {

/// Placeholder for current time point.
struct C_TIME {};
/// Placeholder for current frame.
struct C_FRAME {};

using primitive_types =
    std::variant<bool, long int, double, std::string, C_TIME, C_FRAME>;

/// @brief Node that holds a constant value: boolean, integer, double, or some string.
struct Constant : primitive_types {
  using primitive_types::variant;

  /// Convenience method to check if the constant is a `bool`.
  [[nodiscard]] constexpr bool is_bool() const {
    return std::holds_alternative<bool>(*this);
  }

  /// Convenience method to check if the constant is a `double`.
  [[nodiscard]] constexpr bool is_real() const {
    return std::holds_alternative<double>(*this);
  }

  /// Convenience method to check if the constant is a `int`.
  [[nodiscard]] constexpr bool is_integer() const {
    return std::holds_alternative<long int>(*this);
  }

  /// Convenience method to check if the constant is a `string`.
  [[nodiscard]] constexpr bool is_string() const {
    return std::holds_alternative<std::string>(*this);
  }

  /// Convenience method to check if the constant is the `C_TIME` special value.
  [[nodiscard]] constexpr bool is_c_time() const {
    return std::holds_alternative<C_TIME>(*this);
  }

  /// Convenience method to check if the constant is the `C_FRAME` special value.
  [[nodiscard]] constexpr bool is_c_frame() const {
    return std::holds_alternative<C_FRAME>(*this);
  }
};

/// @brief A typed variable.
///
/// Used as a placeholder until evaluated by the chosen semantics.
struct Variable {
  /// The name of the variable (some qualified identifier).
  std::string name;

  /// The type of the variable can either be a primitive value, or a custom type.
  /// An example of a custom type would be a `DataFrame` in the STQL semantics.
  enum struct Type {
    Frame,     ///< The variable is a frame placeholder.
    Timepoint, ///< The variable is a timepoint placeholder
    Real,      ///< Real-valued (double) variables.
    Int,       ///< Integer variables.
    Bool,      ///< Boolean variables.
    Custom,    ///< Used when the type is some custom type.
    Unknown,   ///< Used when we don't know the type and it needs to be inferred later.
  };
  /// The type of the variable.
  Type type;
  /// If the variable is a custom type, then this field holds the string representation
  /// of the type.
  std::optional<std::string> custom_type;

  /// Scope of the varaible
  ///
  /// Local scope implies it has been declared within a Quantifier expression.
  enum struct Scope { Local, Global };
  Scope scope;

  Variable(
      std::string name_arg,
      Type type_arg,
      std::optional<std::string> type_str = std::nullopt,
      Scope scope_arg                     = Scope::Local) :
      name{std::move(name_arg)},
      type{type_arg},
      custom_type{std::move(type_str)},
      scope{scope_arg} {};

  [[nodiscard]] constexpr bool is_frame() const {
    return type == Type::Frame;
  }
  [[nodiscard]] constexpr bool is_timepoint() const {
    return type == Type::Timepoint;
  }
  [[nodiscard]] constexpr bool is_bool() const {
    return type == Type::Bool;
  }
  [[nodiscard]] constexpr bool is_real() const {
    return type == Type::Real;
  }
  [[nodiscard]] constexpr bool is_integer() const {
    return type == Type::Int;
  }
  [[nodiscard]] constexpr bool is_custom() const {
    return type == Type::Custom;
  }
};

} // namespace PERCEMON_AST_NS

#endif /* end of include guard: __PERCEMON_AST_DETAILS_PRIMITIVES_HPP__ */
