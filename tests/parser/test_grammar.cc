#include <catch2/catch.hpp>

#include "parser/grammar.hpp"

TEST_CASE("PEG Grammar has no issues", "[parser][grammar]") {
  size_t issues = percemon::grammar::internal::analyze(/*verbose=*/1);
  INFO("Number of issues = " << issues);
  REQUIRE(issues == 0);
}
