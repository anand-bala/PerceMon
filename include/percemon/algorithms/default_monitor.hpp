/// @file     percemon/algorithms/default_monitor.hpp
/// @brief    Default (online) STQL monitor.

#pragma once
#ifndef PERCEMON_ALGORITHMS_DEFAULT_MONITOR
#define PERCEMON_ALGORITHMS_DEFAULT_MONITOR

#include "percemon/ast/ast_fwd.hpp"
#include "percemon/datastream.hpp"
#include "percemon/topo.hpp"

#include <deque>

namespace percemon {

inline namespace monitors {

/// @brief Default (online) monitor for STQL.
struct Monitor {
  Monitor(ExprPtr phi, topo::BoundingBox frame_size, double fps);

  /// @brief Add a frame to the monitor buffer.
  void add_frame(const datastream::Frame& frame);
  void add_frame(datastream::Frame&& frame);

  /// @brief Evaluate the robustness for the current buffered frame.
  [[nodiscard]] double eval() const;

 private:
  /// Formula to be monitored
  ExprPtr m_phi;

  /// Bounding box for the frame.
  topo::BoundingBox m_boundary;

  /// Frames per second for the datastream
  double m_fps;

  /// A buffer containing the history of Frames required to compute robustness of phi
  /// efficiently.
  std::deque<datastream::Frame> buffer;

  /// History required for the formula.
  std::optional<size_t> m_history;
  /// Future frames required for the formula.
  std::optional<size_t> m_horizon;

  /// Check if the buffer is dirty, i.e., we haven't computed the robustness for the
  /// current state of the buffer.
  bool is_dirty;
};

} // namespace monitors

} // namespace percemon

#endif /* end of include guard: PERCEMON_ALGORITHMS_DEFAULT_MONITOR */
