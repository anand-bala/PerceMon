#include "percemon/parser.hpp"

#include "grammar.hpp"    // IWYU pragma: keep
#include "parse_tree.hpp" // for transform, Selector, par...
#include "utils/visit.hpp"

#include <tao/pegtl.hpp>                    // for pegtl
#include <tao/pegtl/contrib/parse_tree.hpp> // for basic_node<>::children_t

#include <algorithm> // for max
#include <utility>   // for move

/// Private namespace for the parser core.
namespace {

namespace gm         = percemon::grammar;
namespace parse_tree = percemon::parser::parse_tree;
namespace peg        = tao::pegtl;

/// This function takes in some PEGTL compliant input and outputs a concrete context.
///
/// To do this, it first generates a parse tree using PEGTL's in-built parse_tree
/// generator. Then, it transforms this parse_tree into the concrete context with the
/// abstract syntax tree.
template <typename ParseInput>
std::unique_ptr<percemon::Context> _parse(ParseInput&& input) {
  if (auto root =
          peg::parse_tree::parse<gm::Specification, parse_tree::Selector>(input)) {
    auto parsed_ctx = parse_tree::transform(std::move(root));
  }
  return nullptr;
}

} // namespace

namespace percemon {
std::unique_ptr<Context> Context::from_string(std::string_view input) {
  tao::pegtl::string_input in(input, "from_content");
  return _parse(in);
}

std::unique_ptr<Context> Context::from_file(const fs::path& input) {
  tao::pegtl::file_input in(input);
  return _parse(in);
}

} // namespace percemon
