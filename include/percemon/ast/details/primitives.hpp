/// @file     ast/details/primitives.hpp
/// @brief    Primitive types in STQL

#pragma once

#ifndef __PERCEMON_AST_DETAILS_PRIMITIVES_HPP__
#define __PERCEMON_AST_DETAILS_PRIMITIVES_HPP__

#include <optional>
#include <string>
#include <variant>

namespace percemon::ast::details {

/// @brief Node that holds a constant value: boolean, integer, double, or some string.
struct Constant : std::variant<bool, int, double, std::string> {
  using std::variant<bool, int, double, std::string>::variant;

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
    return std::holds_alternative<int>(*this);
  }

  /// Convenience method to check if the constant is a `string`.
  [[nodiscard]] constexpr bool is_string() const {
    return std::holds_alternative<std::string>(*this);
  }
};

/// @brief A typed variable.
///
/// Used as a placeholder until evaluated by the chosen semantics.
struct Variable {
  /// The type of the variable can either be a primitive value, or a custom type.
  /// An example of a custom type would be a `DataFrame` in the STQL semantics.
  enum struct Type {
    Frame,
    Timepoint,
    Real,
    Int,
    Bool,
    Custom,
  };
  // namespace percemon::ast::details
  /// The name of the variable (some qualified identifier).
  std::string name;

  /// The type of the variable.
  Type type;
  /// If the variable is a custom type, then this field holds the string representation
  /// of the type.
  std::optional<std::string> custom_type;

  Variable(
      std::string name_arg,
      Type type_arg,
      std::optional<std::string> type_str = std::nullopt) :
      name{std::move(name_arg)}, type{type_arg}, custom_type{std::move(type_str)} {};

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

} // namespace percemon::ast::details

#endif /* end of include guard: __PERCEMON_AST_DETAILS_PRIMITIVES_HPP__ */
