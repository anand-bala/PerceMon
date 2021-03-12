#pragma once
#ifndef PERCEMON_UTILS_STATIC_ANALYSIS_HEPLERS_HPP
#define PERCEMON_UTILS_STATIC_ANALYSIS_HEPLERS_HPP

#include <iostream>
#include <string>
#include <type_traits>

// LCOV_EXCL_START

namespace utils {

template <typename... Msg>
inline void assert_([[maybe_unused]] bool condition, [[maybe_unused]] Msg... msg) {
#ifdef NDEBUG
  if (!condition) {
    std::cerr << "Assertion Failed: ";
    (std::cerr << msg << ...);
    std::cerr << std::endl;
    abort();
  }
#endif // NDEBUG
}

[[noreturn]] inline void unreachable() {
  assert_(std::false_type(), "Unreachable code!");
#ifdef _MSC_VER
  __assume(false);
#elif defined(__clang__) || defined(__GNUC__) || defined(__INTEL_COMPILER)
  __builtin_unreachable();
#else
  abort();
#endif
}

[[noreturn]] inline void unreachable(std::string_view msg) {
  std::cerr << "Unreachable: " << msg << std::endl;
  unreachable();
}

} // namespace utils

// LCOV_EXCL_STOP
#endif /* end of include guard: PERCEMON_UTILS_STATIC_ANALYSIS_HEPLERS_HPP */
