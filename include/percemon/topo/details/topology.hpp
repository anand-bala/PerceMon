/// @file     percemon/topo/details/topology.hpp
/// @brief    Define an overarching topological region type.

#pragma once
#ifndef PERCEMON_TOPO_DETAILS_REGION
#define PERCEMON_TOPO_DETAILS_REGION

#include "percemon/topo/topo_fwd.hpp"

// IWYU pragma: begin_exports
#include "percemon/topo/details/base.hpp"
#include "percemon/topo/details/bbox.hpp"
#include "percemon/topo/details/primitives.hpp"
#include "percemon/topo/details/quadtree.hpp"
// IWYU pragma: end_exports

#include <variant>
#include <vector>

namespace percemon::topo {

/// A Naive union of  bounding boxes. Useful when we are dealing with only bounding
/// boxes without knowledge of the larger topology.
///
/// @note This is a hack :D
using BBoxList      = std::vector<BoundingBox>;
using TopologyTypes = std::variant<Empty, Universe, BoundingBox, BBoxList, Quadtree>;

/// @brief A Topological Region
///
/// A topological region is a sum-type consisting of either a primitive topology
/// (UniverseSet or EmptySet), a bounding box, or a quadtree. This allows use to also
/// define high level operations on Regions that will call the corresponding operation
/// on the actual type in the union.
struct Topology : TopologyTypes {
  using TopologyTypes::variant;

  /// Get the area of a topological region
  [[nodiscard]] double area() const;
  /// Compute the spatial union of this region with another.
  [[nodiscard]] Topology merge(const Topology& other) const;
  /// Compute the spatial union of this region with many other regions
  [[nodiscard]] Topology merge(const std::vector<Topology>& others) const;
  /// Compute the spatial intersection of this region with another.
  [[nodiscard]] Topology intersect(const Topology& other) const;
  /// Compute the spatial intersection of this region with many other regions
  [[nodiscard]] Topology intersect(const std::vector<Topology>& others) const;
};

} // namespace percemon::topo

#endif /* end of include guard: PERCEMON_TOPO_DETAILS_REGION */
