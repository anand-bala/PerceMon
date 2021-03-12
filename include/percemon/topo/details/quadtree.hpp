/// @file     percemon/topo/details/quadtree.hpp
/// @brief    Quadtree data structure to hold bounding boxes efficiently

#pragma once
#ifndef PERCEMON_TOPO_DETAILS_QUADTREE
#define PERCEMON_TOPO_DETAILS_QUADTREE

#include "percemon/topo/topo_fwd.hpp"

#include "percemon/topo/details/base.hpp"
#include "percemon/topo/details/bbox.hpp"

#include <array>
#include <cstddef>
#include <memory>
#include <stdexcept>
#include <vector>

namespace percemon::topo {

constexpr size_t QuadtreeMaxDepth = 8;
constexpr double MinBoxSize       = 0.005;

/// @brief A quadtree datastructure that stores the area covered by a union of bounding
/// boxes.
///
/// Design
/// ======
///
/// The design of the quadtree is similar to that used for image compression.
/// Essentially, we have 3 kinds of nodes: Black, White, and Grey. A node is Black if it
/// has been completely covered by the union of bounding boxes, White is it empty, and
/// Grey if it is partially filled.
///
/// Throughout the lifetime of the quadtree, we hold the following invariants:
///
/// - All leaf nodes must be colored Black or White.
/// - The depth of the tree must not exceed `max_depth`
///
/// @note
/// The consequence of the above invariants is that if we try to insert a box that is
/// smaller than the smallest possible leaf node, then the node is automatically colored
/// black. So choose the `max_depth` appropriately. Or, optionally, you can set the
/// `min_size` on the size of the box, which will ignore all boxes that are that small.
struct Quadtree : details::BaseTopo<Quadtree> {
  /// @brief Create a quadtree that encompasses a 2D space of given size.
  Quadtree(
      double width,
      double height,
      size_t max_depth = QuadtreeMaxDepth,
      double min_size  = MinBoxSize) :
      m_root{std::make_unique<Node>(
          0,
          BoundingBox{{}, {width, height}},
          max_depth,
          min_size)} {}

  /// @brief Create a quadtree that encompasses a 2D space of given size.
  Quadtree(
      BoundingBox boundary,
      size_t max_depth = QuadtreeMaxDepth,
      double min_size  = MinBoxSize) :
      m_root{std::make_unique<Node>(0, boundary, max_depth, min_size)} {}

  Quadtree(const Quadtree& other) {
    *this = other;
  }

  Quadtree& operator=(const Quadtree& other) {
    if (this != &other) {
      m_root = std::make_unique<Node>(
          0, other.m_root->boundary, other.m_root->max_depth, other.m_root->min_size);
      m_root->copy_tree(other.m_root);
    }
    return *this;
  }

  Quadtree(Quadtree&&) = default;
  Quadtree& operator=(Quadtree&&) = default;

  ~Quadtree() = default;

  [[nodiscard]] BoundingBox boundary() const {
    return m_root->boundary;
  }

  [[nodiscard]] double width() const {
    return m_root->boundary.width;
  }

  [[nodiscard]] double height() const {
    return m_root->boundary.height;
  }

  [[nodiscard]] size_t max_depth() const {
    return m_root->max_depth;
  }

  [[nodiscard]] double min_size() const {
    return m_root->min_size;
  }

  /// @brief Add a bounding box to the Quadtree
  void insert(BoundingBox box) {
    m_root->insert(box);
  }
  /// @brief Insert a series of bounding boxes
  void insert(const std::vector<BoundingBox>& list) {
    for (const auto& bbox : list) { m_root->insert(bbox); }
  }

  /// Compute the union of two Quadtree.
  [[nodiscard]] Quadtree merge(const Quadtree& other) const {
    if (m_root->boundary.size() != other.m_root->boundary.size()) {
      throw std::invalid_argument("Given Quadtree objects don't have the same size");
    }

    auto [width, height] = m_root->boundary.size();
    auto max_depth       = std::min(m_root->max_depth, other.m_root->max_depth);
    auto min_size        = std::max(m_root->min_size, other.m_root->min_size);
    auto ret             = Quadtree{width, height, max_depth, min_size};

    ret.m_root->merge(m_root, other.m_root);
    return ret;
  }
  /// Compute the intersection of two Quadtree.
  [[nodiscard]] Quadtree intersect(const Quadtree& other) const {
    if (m_root->boundary.size() != other.m_root->boundary.size()) {
      throw std::invalid_argument("Given Quadtree objects don't have the same size");
    }

    auto [width, height] = m_root->boundary.size();
    auto max_depth       = std::min(m_root->max_depth, other.m_root->max_depth);
    auto min_size        = std::max(m_root->min_size, other.m_root->min_size);
    auto ret             = Quadtree{width, height, max_depth, min_size};

    ret.m_root->intersect(m_root, other.m_root);
    return ret;
  }

  [[nodiscard]] double area() const {
    return m_root->area();
  }

 private:
  enum struct Quadrant {
    NorthWest = 0,
    NorthEast = 1,
    SouthWest = 2,
    SouthEast = 3,
    Mixed     = -1, ///< Used when a BoundingBox lies in multiple quadrants
  };

  /// Color of the node
  enum struct Color { Black, White, Grey };

  /// A node in the quadtree, i.e., a quadrant
  struct Node {
    size_t depth;
    /// Boundary box for this node
    BoundingBox boundary;
    /// A child node for each quadrant
    std::array<std::unique_ptr<Node>, 4> children;
    /// The color of the node
    Color color;
    /// If the node a leaf?
    bool is_leaf_p;

    size_t max_depth;
    double min_size;

    Node(
        size_t node_depth,
        BoundingBox bounds,
        size_t maximum_depth = QuadtreeMaxDepth,
        double minimum_size  = MinBoxSize) :
        depth{node_depth},
        boundary{bounds},
        color{Color::White},
        is_leaf_p{true},
        max_depth{maximum_depth},
        min_size{minimum_size} {};

    /// Check if the given node is a leaf node
    [[nodiscard]] bool is_leaf() const {
      return is_leaf_p;
    }

    /// Compute the bounding box for a quadrant in the node.
    [[nodiscard]] BoundingBox compute_box(Quadrant i) const {
      auto origin    = boundary.min_coordinates();
      auto childSize = boundary.size() / static_cast<double>(2);
      switch (i) {
        case Quadrant::NorthWest:
          return BoundingBox{origin, childSize};
        case Quadrant::NorthEast:
          return BoundingBox{{origin.x + childSize.x, origin.y}, childSize};
        case Quadrant::SouthWest:
          return BoundingBox{{origin.x, origin.y + childSize.y}, childSize};
        case Quadrant::SouthEast:
          return BoundingBox{origin + childSize, childSize};
        case Quadrant::Mixed:
          throw std::logic_error("Can't compute the box for a Mixed quadrant type");
      }
    }

    [[nodiscard]] Quadrant get_quadrant(const BoundingBox& value_box) const noexcept {
      auto center = boundary.center();
      if (value_box.xmax() < center.x) {   // West
        if (value_box.ymax() < center.y) { // North
          return Quadrant::NorthWest;
        } else if (value_box.ymin >= center.y) { // South
          return Quadrant::SouthWest;
        } else { // Overlap
          return Quadrant::Mixed;
        }
      } else if (value_box.xmin >= center.x) { // East
        if (value_box.ymax() < center.y) {     // North
          return Quadrant::NorthEast;
        } else if (value_box.ymin >= center.y) { // South
          return Quadrant::SouthEast;
        } else { // Overlap
          return Quadrant::Mixed;
        }
      } else { // Overlap (East-West)
        return Quadrant::Mixed;
      }
    }
    /// Add bounding box to node or to children
    void insert(BoundingBox box);

    /// Split a node into it's quadrants
    void split();

    /// Try to merge the node with it's children.
    void try_merge();

    /// Copy the subtree of the other node.
    void copy_tree(const std::unique_ptr<Node>& other);

    /// Merge two other nodes (at the same depth) into the current tree
    void merge(const std::unique_ptr<Node>& x, const std::unique_ptr<Node>& y);

    /// Intersect two other nodes (at the same depth) into the current tree
    void intersect(const std::unique_ptr<Node>& x, const std::unique_ptr<Node>& y);

    [[nodiscard]] double area() const;
  };
  using NodePtr = std::unique_ptr<Node>;

  NodePtr m_root;
};

} // namespace percemon::topo

#endif /* end of include guard: PERCEMON_TOPO_DETAILS_QUADTREE */
