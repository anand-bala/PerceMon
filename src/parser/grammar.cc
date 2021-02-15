#include <tao/pegtl/contrib/analyze.hpp> // for analyze
#include <tao/pegtl/contrib/trace.hpp>   // for standard_trace
// IWYU pragma: no_include <algorithm>
#include <cstddef> // for size_t

#include "percemon/internal/filesystem.hpp"
#include "percemon/parser.hpp"
#include "grammar.hpp"

// LCOV_EXCL_START

size_t percemon::grammar::internal::analyze(int verbose) {
  return peg::analyze<Specification>(verbose);
}

bool percemon::grammar::internal::trace_from_file(const fs::path& input_path) {
  peg::file_input in(input_path);
  return peg::standard_trace<Specification>(in);
}
// LCOV_EXCL_STOP
