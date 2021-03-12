/// @file     percemon/topo/details/primitives.hpp
/// @brief    Topological primitives

#pragma once
#ifndef PERCEMON_TOPO_DETAILS_PRIMITIVES
#define PERCEMON_TOPO_DETAILS_PRIMITIVES

#include "percemon/topo/details/base.hpp"
#include "percemon/topo/topo_fwd.hpp"

#include <limits>

namespace percemon::topo {

/// @brief An empty region/set
struct Empty : details::BaseTopo<Empty> {
  [[nodiscard]] double area() const {
    return 0;
  }
};

/// @brief The universe set
struct Universe : details::BaseTopo<Universe> {
  [[nodiscard]] double area() const {
    return std::numeric_limits<double>::infinity();
  }
};

} // namespace percemon::topo

#endif /* end of include guard: PERCEMON_TOPO_DETAILS_PRIMITIVES */
