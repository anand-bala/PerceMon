/// @file     percemon/algorithms/default_monitor.hpp
/// @brief    Default (online) STQL monitor.

#pragma once
#ifndef PERCEMON_ALGORITHMS_DEFAULT_MONITOR
#define PERCEMON_ALGORITHMS_DEFAULT_MONITOR

#include "percemon/ast/ast_fwd.hpp"
#include "percemon/datastream.hpp"
#include "percemon/topo.hpp"

#include <cmath>
#include <deque>

namespace percemon {

inline namespace monitors {

/// @brief Default (online) monitor for STQL.
struct Monitor {
  Monitor(
      ExprPtr phi,
      topo::BoundingBox frame_size,
      double fps = datastream::DEFAULT_FPS);

  /// @brief Add a frame to the monitor buffer.
  void add_frame(const datastream::Frame& frame);
  void add_frame(datastream::Frame&& frame);

  /// @brief Evaluate the robustness for the current buffered frame.
  [[nodiscard]] double eval();

 private:
  struct Impl;

  /// Pointer to the actual implementation
  std::unique_ptr<Impl> m_impl;
};

} // namespace monitors

} // namespace percemon

#endif /* end of include guard: PERCEMON_ALGORITHMS_DEFAULT_MONITOR */
