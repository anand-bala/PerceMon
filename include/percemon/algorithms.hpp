/// @brief Common interface file for algorithms
#pragma once
#ifndef PERCEMON_ALGORITHMS
#define PERCEMON_ALGORITHMS

// IWYU pragma: begin_exports
#include "percemon/algorithms/default_monitor.hpp"
// IWYU pragma: end_exports
#include "percemon/ast/ast_fwd.hpp"
#include "percemon/datastream.hpp"

namespace percemon {

inline namespace monitor_info {
/// @brief Data structure to hold information for monitoring buffer.
struct MonitorInfo {
  std::optional<size_t> history; ///< The number of past frames required
  std::optional<size_t> horizon; ///< The number of future frames required

  static MonitorInfo get(const ExprPtr& expr, double fps = datastream::DEFAULT_FPS);
};

} // namespace monitor_info

} // namespace percemon

#endif /* end of include guard: PERCEMON_ALGORITHMS */
