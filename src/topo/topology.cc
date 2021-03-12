#include "percemon/topo/details/topology.hpp"

#include "utils/static_analysis_helpers.hpp"
#include "utils/visit.hpp"

#include <range/v3/view.hpp>
#include <range/v3/view/cartesian_product.hpp>

#include <stdexcept>

namespace percemon::topo {

double Topology::area() const {
  return utils::visit(
      utils::overloaded{
          [](const BBoxList& list) {
            double ret = 0.0;
            for (const auto& bbox : list) { ret += bbox.area(); }
            return ret;
          },
          [](const auto& region) {
            return region.area();
          }},
      *this);
}

Topology Topology::merge(const Topology& other) const {
  const auto op = utils::overloaded{
      [](const Quadtree& q1, const Quadtree& q2) -> Topology { return q1.merge(q2); },
      [](Quadtree q, const BBoxList& list) -> Topology {
        q.insert(list);
        return q;
      },
      [](Quadtree q, const BoundingBox& bbox) -> Topology {
        q.insert(bbox);
        return q;
      },
      [](const Quadtree&, const Universe&) -> Topology { return Universe{}; },
      [](Quadtree q, const Empty&) -> Topology { return q; },
      [](const BBoxList& list, Quadtree q) -> Topology {
        q.insert(list);
        return q;
      },
      [](BBoxList b1, const BBoxList& b2) -> Topology {
        b1.insert(b1.begin(), b2.begin(), b2.end());
        return b1;
      },
      [](BBoxList list, const BoundingBox& bbox) -> Topology {
        list.push_back(bbox);
        return list;
      },
      [](const BBoxList&, const Universe&) -> Topology { return Universe{}; },
      [](BBoxList list, const Empty&) -> Topology { return list; },
      [](const BoundingBox& bbox, Quadtree q) -> Topology {
        q.insert(bbox);
        return q;
      },
      [](const BoundingBox& bbox, BBoxList list) -> Topology {
        list.push_back(bbox);
        return list;
      },
      [](const BoundingBox& b1, const BoundingBox& b2) -> Topology {
        return BBoxList{b1, b2};
      },
      [](const BoundingBox&, const Universe&) -> Topology { return Universe{}; },
      [](BoundingBox b, const Empty&) -> Topology { return b; },
      [](Universe, auto&) -> Topology { return Universe{}; },
      [](Empty, auto r) -> Topology { return r; },
  };
  return utils::visit(op, *this, other);
}

Topology Topology::merge(const std::vector<Topology>& others) const {
  Topology ret = *this;
  for (const auto& other : others) { ret = ret.merge(other); }
  return ret;
}

Topology Topology::intersect(const Topology& other) const {
  const auto op = utils::overloaded{
      [](const Quadtree& q1, const Quadtree& q2) -> Topology {
        return q1.intersect(q2);
      },
      [](const Quadtree& q, const BBoxList& list) -> Topology {
        auto q2 = Quadtree{q.boundary(), q.max_depth(), q.min_size()};
        q2.insert(list);
        return q.intersect(q2);
      },
      [](const Quadtree& q, const BoundingBox& bbox) -> Topology {
        auto q2 = Quadtree{q.boundary(), q.max_depth(), q.min_size()};
        q2.insert(bbox);
        return q.intersect(q2);
      },
      [](Quadtree q, const Universe&) -> Topology { return q; },
      [](const Quadtree&, const Empty&) -> Topology { return Empty{}; },
      [](const BBoxList& list, const Quadtree& q) -> Topology {
        auto q2 = Quadtree{q.boundary(), q.max_depth(), q.min_size()};
        q2.insert(list);
        return q.intersect(q2);
      },
      [](const BBoxList& l1, const BBoxList& l2) -> Topology {
        // utils::assert_(
        //     std::false_type(),
        //     "Trying to intersect two BoundingBox lists. This should never be called
        //     as it is computationally intensive.");
        auto ret = BBoxList{};
        ret.reserve(l1.size() * l2.size());
        for (const auto& [b1, b2] : ranges::views::cartesian_product(l1, l2)) {
          ret.push_back(b1.intersect(b2));
        }
        return ret;
      },
      [](BBoxList list, const BoundingBox& bbox) -> Topology {
        for (auto& box : list) { box = bbox.intersect(box); }
        return list;
      },
      [](BBoxList b, const Universe&) -> Topology { return b; },
      [](const BBoxList&, const Empty&) -> Topology { return Empty{}; },
      [](const BoundingBox& bbox, const Quadtree& q) -> Topology {
        auto q2 = Quadtree{q.boundary(), q.max_depth(), q.min_size()};
        q2.insert(bbox);
        return q.intersect(q2);
      },
      [](const BoundingBox& bbox, BBoxList list) -> Topology {
        for (auto& box : list) { box = bbox.intersect(box); }
        return list;
      },
      [](const BoundingBox& b1, const BoundingBox& b2) -> Topology {
        return b1.intersect(b2);
      },
      [](BoundingBox b, const Universe&) -> Topology { return b; },
      [](const BoundingBox&, const Empty&) -> Topology { return Empty{}; },
      [](Universe, auto r) -> Topology { return r; },
      [](Empty, auto&) -> Topology { return Empty{}; },
  };
  return utils::visit(op, *this, other);
}

Topology Topology::intersect(const std::vector<Topology>& others) const {
  Topology ret = *this;
  for (const auto& other : others) { ret = ret.intersect(other); }
  return ret;
}

} // namespace percemon::topo
