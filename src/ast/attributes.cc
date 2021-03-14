#include "percemon/ast/details/attributes.hpp"

#include "utils/visit.hpp"

#include <fmt/format.h>

#include <string>

using namespace percemon::ast::details;

std::string Attribute::to_string() const {
  if (values.empty() ||
      (values.size() == 1 && std::holds_alternative<bool>(values.back()))) {
    return fmt::format(":{}", key);
  }
  constexpr auto format = utils::overloaded{
      [](const std::string& s) { return fmt::format("\"{}\"", s); },
      [](auto c) { return fmt::format("{}", c); },
  };
  auto values_str = std::vector<std::string>{};
  for (const auto& v : values) { values_str.push_back(utils::visit(format, v)); }
  return fmt::format(":{} ({})", key, fmt::join(values_str, " "));
}
