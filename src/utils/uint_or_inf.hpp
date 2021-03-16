/// @file     utils/uint_or_inf.hpp
/// @brief    Utilities for Percemon algorithms

#pragma once
#ifndef PERCEMON_ALGORITHMS_UTILITY
#define PERCEMON_ALGORITHMS_UTILITY

#include <cmath>
#include <cstddef>
#include <functional>
#include <limits>
#include <stdexcept>
#include <type_traits>

#include "operators.hpp"

namespace utils {

namespace taoops = tao::operators;

/// @brief A wrapper type for the set of Integers + Infinity.
struct UintOrInf : taoops::partially_ordered<UintOrInf>,
                   taoops::partially_ordered<UintOrInf, size_t>,
                   taoops::partially_ordered<UintOrInf, float>,
                   taoops::partially_ordered<UintOrInf, double>,
                   taoops::commutative_addable<UintOrInf>,
                   taoops::commutative_addable<UintOrInf, size_t>,
                   taoops::subtractable<UintOrInf, size_t> {
  template <typename Num>
  UintOrInf(Num val) {
    static_assert(std::is_arithmetic<Num>::value);
    if constexpr (std::is_integral_v<Num>) {
      m_value     = static_cast<size_t>(val);
      m_is_finite = true;
    } else if constexpr (std::is_floating_point_v<Num>) {
      val = std::round(val);
      if (val >= 0) {
        if (std::isfinite(val)) {
          m_value     = static_cast<size_t>(val);
          m_is_finite = true;
        } else {
          m_value     = static_cast<size_t>(-1);
          m_is_finite = false;
        }
      } else {
        throw std::invalid_argument("UintOrInf cannot have negative value");
      }
    }
  }

  UintOrInf()                 = default;
  UintOrInf(const UintOrInf&) = default;
  UintOrInf(UintOrInf&&)      = default;
  UintOrInf& operator=(const UintOrInf&) = default;
  UintOrInf& operator=(UintOrInf&&) = default;

  static UintOrInf infinity() {
    auto ret        = UintOrInf{static_cast<size_t>(-1)};
    ret.m_is_finite = false;
    return ret;
  }

  [[nodiscard]] constexpr size_t value() const {
    return m_value;
  }
  [[nodiscard]] constexpr bool is_finite() const {
    return m_is_finite;
  }

  [[nodiscard]] constexpr bool is_infinite() const {
    return !m_is_finite;
  }

  [[nodiscard]] constexpr bool operator==(const UintOrInf& rhs) const {
    if (is_finite()) {
      if (rhs.is_finite()) {
        // just check the values.
        return m_value == rhs.m_value;
      } else {
        return false;
      }
    } else { // we are infinite
      return rhs.is_infinite();
    }
  }

  template <typename Num>
  [[nodiscard]] constexpr bool operator==(const Num& rhs) const {
    static_assert(
        std::is_arithmetic_v<Num>,
        "UintOrInf can only be compared with arithmetic types");
    if (is_finite()) {
      // Just check the values.
      return m_value == rhs;
    } else if (std::numeric_limits<Num>::has_infinity) { // We are infinity after this
      return std::numeric_limits<Num>::infinity() == rhs;
    } else {
      // Finite values cannot be infinity
      return false;
    }
  }

  [[nodiscard]] constexpr bool operator<(const UintOrInf& rhs) const {
    if (is_finite()) {
      if (rhs.is_finite()) {
        // just check the values.
        return m_value < rhs.m_value;
      } else {
        // RHS (Infinite) is larger than LHS (Finite)
        return false;
      }
    } else { // we are infinite
      // lhs !< rhs if both are infinite
      return !rhs.is_infinite();
    }
  }

  template <typename Num>
  [[nodiscard]] constexpr bool operator<(const Num& rhs) const {
    static_assert(
        std::is_arithmetic_v<Num>,
        "UintOrInf can only be compared with arithmetic types");
    if (is_finite()) {
      // Just check the values.
      return m_value < rhs;
    } else if (std::numeric_limits<Num>::has_infinity) { // We are infinity after this
      return std::numeric_limits<Num>::infinity() < rhs;
    } else {
      // Infinity is never less than finite values
      return false;
    }
  }

  template <typename Num>
  [[nodiscard]] constexpr bool operator>(const Num& rhs) const {
    static_assert(
        std::is_arithmetic_v<Num>,
        "UintOrInf can only be compared with arithmetic types");
    if (is_finite()) {
      // Just check the values.
      return m_value > rhs;
    } else if (std::numeric_limits<Num>::has_infinity) { // We are infinity after this
      return std::numeric_limits<Num>::infinity() > rhs;
    } else {
      // Infinity is never less than finite values
      return true;
    }
  }

  UintOrInf& operator+=(const UintOrInf& rhs) {
    if (is_finite()) {
      if (rhs.is_infinite()) {
        // This becomes infinite.
        *this = rhs;
      } else {
        m_value += rhs.m_value;
      }
    }
    // Do nothing otherwise.
    return *this;
  }

  template <typename Num>
  UintOrInf& operator+=(const Num& rhs) {
    static_assert(
        std::is_arithmetic_v<Num>, "UintOrInf can only be added to arithmetic types");
    if (is_finite()) {
      // Just add
      m_value += rhs;
    }
    // Do nothing otherwise.
    return *this;
  }

  template <typename Num>
  UintOrInf& operator-=(const Num& rhs) {
    static_assert(
        std::is_arithmetic_v<Num>,
        "UintOrInf can only be subtracted with arithmetic types");
    if (is_finite()) {
      // Just subtract
      m_value -= rhs;
    }
    // Do nothing otherwise.
    return *this;
  }

 private:
  size_t m_value   = 0;
  bool m_is_finite = true;
};

} // namespace utils

#endif /* end of include guard: PERCEMON_ALGORITHMS_UTILITY */
