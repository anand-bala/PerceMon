#include <catch2/catch.hpp>

#include "Argus/parser.hpp"

TEST_CASE("PEG Grammar has no issues", "[parser][grammar]") {
  size_t issues = argus::grammar::internal::analyze(1);
  INFO("Number of issues = " << issues);
  REQUIRE(issues == 0);
}
