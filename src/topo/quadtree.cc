#include "percemon/topo/details/quadtree.hpp"

#include "utils/static_analysis_helpers.hpp"

namespace percemon::topo {
void Quadtree::Node::insert(BoundingBox box) {
  // If the box is too small or if the node is already filled, we don't do anything
  if (box.area() <= min_size || color == Color::Black) {
    return;
  }

  // If we are in too deep, we will just paint the node black and stop.
  if (depth >= max_depth) {
    color = Color::Black;
    return;
  }

  // It only makes sense to add a box that is contained within this node.
  utils::assert_(
      boundary.contains(box), "Attempting to add a box that is outside a node");
  // Here, we know that it is actually useful to add the box to the tree

  // Let us check if the box completely covers the node
  if (boundary.intersect(box).area() == boundary.area()) {
    // If it does, we can just paint the node Black.
    color = Color::Black;
    return;
  }

  // If it doesn't cover the node, then we need to split up the box into each node
  // quadrant.
  split();
  // Not all children should be White.
  // Try to paint each child quadrant with the box.
  for (int i = 0; i < 4; i++) {
    utils::assert_(
        children.at(i) != nullptr, "Children nodes havent been created after split");
    auto quadrant = static_cast<Quadrant>(i);

    // get the bounding box for the quadrant
    auto quad_bbox = compute_box(quadrant);
    // Compute the intersection of the quadrant and the input box
    auto intersect_box = quad_bbox.intersect(box);
    // Add the intersecting box to the quadrant.
    children.at(i)->insert(intersect_box);
    // If the child node isn't white anymore, paint the node grey
    if (children.at(i)->color != Color::White) {
      color = Color::Grey;
    }
  }

  // Check if the subtree can be merged
  try_merge();
}

void Quadtree::Node::split() {
  utils::assert_(color != Color::Black, "Someone is trying to split a Black node");
  // Check if we already split the node
  if (!is_leaf()) {
    return;
  }
  // Otherwise, we just initialize each child quadrant to a White node
  for (int i = 0; i < 4; i++) {
    utils::assert_(
        children.at(i) == nullptr,
        "Looks like someone initialized the child of a leaf node without using `split()`.");
    auto quadrant = static_cast<Quadrant>(i);
    // get the bounding box for the quadrant
    auto quad_bbox = compute_box(quadrant);
    children.at(i) = std::make_unique<Node>(depth + 1, quad_bbox, max_depth, min_size);
  }
  is_leaf_p = false;
}

void Quadtree::Node::try_merge() {
  // We are going to try and merge an entire Quadtree subtree if it is filled (but we
  // just don't know it.

  // If it is already merged, do nothing
  if (is_leaf()) {
    return;
  }
  // So, we just check if all 4 children are all black or all white.
  // We can recursively run merge on each child, and check if the output is white or
  // black.
  bool all_black = true;
  bool all_white = true;
  for (int i = 0; i < 4; i++) {
    children.at(i)->try_merge();
    all_black = all_black && (children.at(i)->color == Color::Black);
    all_white = all_white && (children.at(i)->color == Color::White);
  }
  if (all_black || all_white) {
    // Set the node to black and make it a leaf node.
    color     = (all_black) ? Color::Black : Color::White;
    is_leaf_p = true;
    for (auto& ptr : children) { ptr = nullptr; }
    return;
  }
  // Since neither all_black or all_white, we can't merge.
  return;
}

void Quadtree::Node::merge(
    const std::unique_ptr<Node>& x,
    const std::unique_ptr<Node>& y) {
  // x and y must be at the same depth. Assume that they represent the same
  // region/quadrant at that depth. They should, by construction.
  utils::assert_(x->depth == y->depth, "Merging nodes must be at the same depth");

  // If either of the nodes are black
  if (x->color == Color::Black || y->color == Color::Black) {
    // The resultant node is definitely black. And we are done
    this->color     = Color::Black;
    this->is_leaf_p = true;
    for (auto& ptr : children) { ptr = nullptr; }
    return;
  }
  // If either is white, the other is copied over
  if (x->color == Color::White) {
    this->copy_tree(y);
    return;
  } else if (y->color == Color::White) {
    this->copy_tree(x);
    return;
  }

  // Now we know that neither of them are White or Black, i.e., both are Grey.
  // Check this just in case
  if (x->color == Color::Grey && y->color == Color::Grey) {
    // Set the resultant node to Grey
    this->color = Color::Grey;
    // Split this node
    this->split();
    // Merge each quadrant recursively
    for (int i = 0; i < 4; i++) {
      children.at(i)->merge(x->children.at(i), y->children.at(i));
    }
    // Try to simplify this node
    try_merge();
    return;
  }
  utils::unreachable("All cases should have been handled");
}

void Quadtree::Node::intersect(
    const std::unique_ptr<Node>& x,
    const std::unique_ptr<Node>& y) {
  // Intersecting is the same as Union, but we swap Black with White.

  // x and y must be at the same depth. Assume that they represent the same
  // region/quadrant at that depth. They should, by construction.
  utils::assert_(x->depth == y->depth, "Merging nodes must be at the same depth");

  // If either of the nodes are white
  if (x->color == Color::White || y->color == Color::White) {
    // The resultant node is definitely white. And we are done
    this->color     = Color::White;
    this->is_leaf_p = true;
    for (auto& ptr : children) { ptr = nullptr; }
    return;
  }
  // If either is black, the other is copied over
  if (x->color == Color::Black) {
    this->copy_tree(y);
    return;
  } else if (y->color == Color::Black) {
    this->copy_tree(x);
    return;
  }

  // Now we know that neither of them are White or Black, i.e., both are Grey.
  // Check this just in case
  if (x->color == Color::Grey && y->color == Color::Grey) {
    // Set the resultant node to Grey
    this->color = Color::Grey;
    // Split this node
    this->split();
    // Merge each quadrant recursively
    for (int i = 0; i < 4; i++) {
      children.at(i)->intersect(x->children.at(i), y->children.at(i));
    }
    // Try to simplify this node
    try_merge();
    return;
  }
  utils::unreachable("All cases should have been handled");
}

void Quadtree::Node::copy_tree(const std::unique_ptr<Node>& other) {
  // Since we are dealing with unique_ptr, we need to deep copy the subtree.
  // First perform a sanity check
  utils::assert_(
      this->depth == other->depth,
      "Nodes must have the same depth when copying subtrees");
  // Copy the color
  this->color     = other->color;
  this->is_leaf_p = other->is_leaf_p;
  // If the thing is a leaf, we don't have to do anything.
  // Otherwise, we need to recursively copy.
  if (!is_leaf_p) {
    // Split this node
    split();
    // For each child, copy the subtree
    for (int i = 0; i < 4; i++) { children.at(i)->copy_tree(other->children.at(i)); }
  }
}

[[nodiscard]] double Quadtree::Node::area() const {
  // The area of a quadtree is the sum of the areas of the black nodes.
  // Let's do this recursively
  switch (color) {
    case Color::Black:
      return this->boundary.area();
    case Color::White:
      return 0.0;
    case Color::Grey:
      // Handle it outside the switch
      break;
  }

  // For each child, compute the area.
  double ret = 0.0;
  for (const auto& child : children) { ret += child->area(); }
  return ret;
}

} // namespace percemon::topo
