/// @file     percemon/topo/details/base.hpp
/// @brief    CRTP base interface for Topological constructs

#pragma once
#ifndef PERCEMON_TOPO_DETAILS_BASE
#define PERCEMON_TOPO_DETAILS_BASE

#include <type_traits>

namespace percemon::topo::details {
template <typename T>
struct BaseTopo {
  /// @brief Get the area of the topological structure
  [[nodiscard]] double area() const {
    return static_cast<T&>(*this).area();
  }

 private:
  friend T;
  BaseTopo() = default;
};
} // namespace percemon::topo::details

#endif /* end of include guard: PERCEMON_TOPO_DETAILS_BASE */
