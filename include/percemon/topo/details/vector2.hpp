/// @file     percemon/topo/details/vector2.hpp
/// @brief    2D Vector (for implementing topological concepts)

#pragma once
#ifndef PERCEMON_TOPO_DETAILS_VECTOR2
#define PERCEMON_TOPO_DETAILS_VECTOR2

#include <utility>

namespace percemon::topo {
template <typename T>
struct Vector2 {
  T x;
  T y;

  constexpr Vector2<T>(T X = 0, T Y = 0) noexcept : x{X}, y{Y} {}

  constexpr Vector2<T>& operator+=(const Vector2<T>& other) noexcept {
    x += other.x;
    y += other.y;
    return *this;
  }

  constexpr Vector2<T>& operator+=(Vector2<T>&& other) noexcept {
    x += other.x;
    y += other.y;
    return *this;
  }

  constexpr Vector2<T>& operator/=(T t) noexcept {
    x /= t;
    y /= t;
    return *this;
  }

  constexpr bool operator==(Vector2<T> other) noexcept {
    return x == other.x && y == other.y;
  }

  constexpr bool operator!=(Vector2<T> other) noexcept {
    return !(*this == other);
  }
};

template <typename T>
constexpr Vector2<T> operator+(Vector2<T> lhs, const Vector2<T>& rhs) noexcept {
  lhs += rhs;
  return lhs;
}

template <typename T>
constexpr Vector2<T> operator+(Vector2<T> lhs, Vector2<T>&& rhs) noexcept {
  lhs += std::move(rhs);
  return lhs;
}

template <typename T>
constexpr Vector2<T> operator/(Vector2<T> vec, T t) noexcept {
  vec /= t;
  return vec;
}
} // namespace percemon::topo

#endif /* end of include guard: PERCEMON_TOPO_DETAILS_VECTOR2 */
