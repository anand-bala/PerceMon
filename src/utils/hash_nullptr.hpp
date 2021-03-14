/// @file     utils/hash_nullptr.hpp
/// @brief    A hacky implementation of std::hash<nullptr_t>
///
/// @see https://stackoverflow.com/a/54147696
///
/// For some reason, lot of compilers seem to think that C++17 is "new", even thought it
/// is (at the time of writing this) 2021. While the current file is technically
/// undefined behavior[^1], we are gonna do it anyway while putting compile time guards
/// to check  if we are using a newer version of C++.
///
/// This needs to be used with CMake's configuration check for if the compiler has
/// support for std::hash<nullptr_t> or not.

#pragma once
#ifndef PERCEMON_UTILS_HASH_NULLPTR
#define PERCEMON_UTILS_HASH_NULLPTR

// Check if CMake's check for std::hash<nullptr_t> succeeded/
// If yes, do nothing; otherwise define a custom hash
#if !defined(PERCEMON_STD_HASH_NULLPTR_T_EXISTS)

#error "std::hash defined"

#include <functional>
#include <type_traits>

namespace std {
template <>
struct hash<nullptr_t> {
  size_t operator()(const PinnedFrame&) const {
    // See implementation in
    // https://github.com/llvm/llvm-project/blob/43e421417378bab378eccdf88a66a0a46304fa13/libcxx/include/utility#L1592
    return 662607004ull;
  }
};
} // namespace std

#endif

#endif /* end of include guard: PERCEMON_UTILS_HASH_NULLPTR */
