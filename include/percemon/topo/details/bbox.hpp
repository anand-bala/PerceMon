/// @file   percemon/topo/detauls/bbox.hpp
/// @brief  A bounding box structure

#pragma once
#ifndef PERCEMON_TOPO_DETAILS_BBOX
#define PERCEMON_TOPO_DETAILS_BBOX

#include "percemon/topo/details/base.hpp"
#include "percemon/topo/details/vector2.hpp"

#include <stdexcept>

namespace percemon::topo {

/// @brief A bounding box structure
///
/// @note The origin in an image is the top left corner. But this structure doesn't
/// really make any assumptions.
struct BoundingBox : details::BaseTopo<BoundingBox> {
  /// Lower X-coordinate
  double xmin;
  /// Lower Y-coordinate
  double ymin;
  /// Width of the box
  double width;
  /// Height of the box
  double height;

  constexpr BoundingBox(const BoundingBox&) = default;
  constexpr BoundingBox(BoundingBox&&)      = default;
  BoundingBox& operator=(const BoundingBox&) = default;
  BoundingBox& operator=(BoundingBox&&) = default;

  constexpr BoundingBox(
      double xmin_   = 0,
      double ymin_   = 0,
      double width_  = 0,
      double height_ = 0) :
      xmin{xmin_}, ymin{ymin_}, width{width_}, height{height_} {};

  constexpr BoundingBox(Vector2<double> min_coord, Vector2<double> size) :
      xmin{min_coord.x}, ymin{min_coord.y}, width{size.x}, height{size.y} {
    if (width < 0 || height < 0) {
      throw std::invalid_argument("Width and height of the Box cannot be negative");
    }
  }

  [[nodiscard]] constexpr double area() const noexcept {
    return height * width;
  }

  [[nodiscard]] constexpr double xmax() const noexcept {
    return xmin + width;
  }

  [[nodiscard]] constexpr double ymax() const noexcept {
    return ymin + height;
  }

  [[nodiscard]] constexpr Vector2<double> min_coordinates() const noexcept {
    return Vector2<double>{xmin, ymin};
  }

  [[nodiscard]] constexpr Vector2<double> center() const noexcept {
    return Vector2<double>{xmin + width / 2, ymin + height / 2};
  }

  [[nodiscard]] constexpr Vector2<double> size() const noexcept {
    return Vector2<double>{width, height};
  }

  [[nodiscard]] constexpr bool contains(const BoundingBox& box) const noexcept {
    return xmin <= box.xmin && box.xmax() <= xmax() && ymin <= box.ymin &&
           box.ymax() <= ymax();
  }

  [[nodiscard]] constexpr bool overlaps(const BoundingBox& box) const noexcept {
    return !(
        xmin >= box.xmax() || xmax() <= box.xmin || ymin >= box.ymax() ||
        ymax() <= box.ymin);
  }

  [[nodiscard]] constexpr BoundingBox
  intersect(const BoundingBox& other) const noexcept {
    if (!overlaps(other)) {
      return BoundingBox{};
    }
    auto origin = Vector2<double>{};
    auto size   = Vector2<double>{};

    if (other.xmin <= xmax()) { // Left of Other within This
      origin.x = other.xmin;
      size.x   = xmax() - other.xmin;
    } else { // Left of This within Other
      origin.x = xmin;
      size.x   = other.xmax() - xmin;
    }

    if (other.ymin <= ymax()) { // Top of Other within This
      origin.y = other.ymin;
      size.y   = ymax() - other.ymin;
    } else { // Top of This within Other
      origin.y = ymin;
      size.y   = other.ymax() - ymin;
    }

    return BoundingBox{origin, size};
  }
};

} // namespace percemon::topo

#endif /* end of include guard: PERCEMON_TOPO_DETAILS_BBOX */
