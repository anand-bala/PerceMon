/// @file     percemon/topo/topo_fwd.hpp
/// @brief    Forward-declarations for Topological constructs

#pragma once
#ifndef PERCEMON_TOPO_FWD_DECL
#define PERCEMON_TOPO_FWD_DECL

#include <memory>

namespace percemon::topo {
struct Empty;
struct Universe;
struct BoundingBox;
struct Quadtree;

struct Topology;
using TopologyPtr = std::shared_ptr<Topology>;
} // namespace percemon::topo

#endif /* end of include guard: PERCEMON_TOPO_FWD_DECL */
