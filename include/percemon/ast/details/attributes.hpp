/// @file     percemon/ast/attributes.hpp
/// @brief    AST nodes for storing attributes/options passed to Commands and Functions.

#pragma once
#ifndef PERCEMON_AST_ATTRIBUTES
#define PERCEMON_AST_ATTRIBUTES

#include "percemon/ast/ast_fwd.hpp"

#include <string> // for operator<, string
#include <variant>
#include <vector> // for vector

namespace percemon::ast::details {

using attribute_value = std::variant<bool, long long int, double, std::string>;

struct Attribute {
  std::string key;
  std::vector<attribute_value> values;

  struct KeyCompare {
    bool operator()(const Attribute& lhs, const Attribute& rhs) const {
      return lhs.key < rhs.key;
    }

    bool operator()(const Attribute& lhs, const std::string& rhs) const {
      return lhs.key < rhs;
    }

    bool operator()(const std::string& lhs, const Attribute& rhs) const {
      return lhs < rhs.key;
    }
  };

  [[nodiscard]] std::string to_string() const;
};

} // namespace percemon::ast::details

#endif /* end of include guard: PERCEMON_AST_ATTRIBUTES */
