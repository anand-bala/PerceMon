#include "percemon/parser.hpp"
#include "parse_tree.hpp"

#include <tao/pegtl/contrib/parse_tree.hpp>

#include <memory>

namespace percemon::parser::parse_tree {

std::unique_ptr<percemon::Context>
transform(std::unique_ptr<tao::pegtl::parse_tree::node>) {
  auto ctx = std::make_unique<Context>();
  return ctx;
}

} // namespace percemon::parser::parse_tree
