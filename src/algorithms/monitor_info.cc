#include "percemon/algorithms.hpp"
#include "percemon/ast.hpp"

#include "utils/static_analysis_helpers.hpp"
#include "utils/uint_or_inf.hpp"
#include "utils/visit.hpp"

#include <algorithm>
#include <stdexcept>

#include <range/v3/algorithm/max_element.hpp>
#include <range/v3/view.hpp>
#include <range/v3/view/transform.hpp>

namespace {

using namespace percemon::ast::details;
using percemon::Expr;
using percemon::ExprPtr;
using percemon::ast::Interval;
using percemon::ast::IntervalPtr;
namespace rv = ranges::views;

struct HorizonCompute {
  struct Info {
    utils::UintOrInf horizon = 0;
    utils::UintOrInf history = 0;
  };

  double fps = percemon::datastream::DEFAULT_FPS;

  /// Cache.
  std::map<size_t, Info> cache;

  HorizonCompute() = default;
  HorizonCompute(double fps_) : fps{fps_}, cache{} {};

  Info eval(const ExprPtr& expr) {
    const size_t id = expr->id();
    const auto iter = cache.find(id);
    if (iter != cache.end() &&
        id != 0) { // Make sure we have a valid ID and it is there in cache.
      return iter->second;
    }
    Info ret  = utils::visit(*this, *expr);
    cache[id] = ret;
    return ret;
  }

  /// Overload for nullptrs
  utils::UintOrInf operator()(std::nullptr_t) {
    return 0;
  }

  template <typename Node>
  Info operator()(const Node&) {
    static_assert(
        std::is_same_v<Node, Constant> || std::is_same_v<Node, Variable> ||
        std::is_same_v<Node, PredicateOp> || std::is_same_v<Node, Function>);
    // If you are using a Predicate or Function for intervals, don't... Use
    // ast::Interval instead as it is more clear for inferring time bounds. Use
    // predicates within subexpressions if you want to use the pinned variables.
    return {0, 0};
  }

  /// @brief We need to propagate the horizons of the subformulae
  Info operator()(const LogicalOp& expr) {
    switch (expr.op) {
      case LogicalOp::Type::Not:
        return this->eval(expr.args.back());
      default:
        break; // Handle it outside.
    }
    // For AND it is the max of each subexpression
    Info info = {0, 0};
    for (const ExprPtr& arg : expr.args) {
      Info tmp     = this->eval(arg);
      info.history = std::max(info.history, tmp.history);
      info.horizon = std::max(info.horizon, tmp.horizon);
    }
    return info;
  }

  Info operator()(const QuantifierOp& expr) {
    return this->eval(expr.arg);
  }

  Info operator()(const PinnedFrame& expr) {
    return this->eval(expr.arg);
  }

  utils::UintOrInf eval(const Interval& interval) {
    // Just check the high
    utils::assert_(
        interval.high != nullptr,
        "If interval was constructed properly, this should be true");
    utils::assert_(std::holds_alternative<Constant>(*interval.high));
    const auto high_expr        = std::get<Constant>(*interval.high);
    const utils::UintOrInf high = utils::visit(
        utils::overloaded{
            [](const std::string&) { return utils::UintOrInf{/*val*/ 0}; },
            [](const bool&) { return utils::UintOrInf{/*val*/ 0}; },
            [](const C_FRAME&) { return utils::UintOrInf{/*val*/ 0}; },
            [](const C_TIME&) { return utils::UintOrInf{/*val*/ 0}; },
            [](const auto& c) { return utils::UintOrInf{/*val*/ c}; },
        },
        high_expr);
    return high;
  }

  Info operator()(const TemporalOp& expr) {
    Info info     = {0, 0};
    auto interval = (expr.interval != nullptr) ? this->eval(*expr.interval)
                                               : utils::UintOrInf::infinity();
    switch (expr.op) {
      case TemporalOp::Type::Next:
        info = this->eval(expr.args.back());
        info.horizon += 1;
        return info;
      case TemporalOp::Type::Previous:
        info = this->eval(expr.args.back());
        info.history += 1;
        return info;
      case TemporalOp::Type::Eventually:
      case TemporalOp::Type::Always:
        info = this->eval(expr.args.back());
        info.horizon += interval;
        return info;
      case TemporalOp::Type::Once:
      case TemporalOp::Type::Historically:
        info = this->eval(expr.args.back());
        info.history += interval;
        return info;
      case TemporalOp::Type::Until:
      case TemporalOp::Type::Since:
        // Handle separately
        break;
    }
    // We know that it is either Since or Until.
    // The hist and hrz equations for the two are identical, but flipped. So let's just
    // do that.
    Info left  = this->eval(expr.args[0]);
    Info right = this->eval(expr.args[1]);
    // Assume it is Until
    info.horizon = std::max(left.horizon + interval - 1, right.horizon + interval);
    info.history = std::max(left.history, right.history);
    // If it is Since, swap
    if (expr.op == TemporalOp::Type::Since) {
      std::swap(info.history, info.horizon);
    }
    return info;
  }

  Info operator()(const SpatialOp& expr) {
    Info info = {0, 0};
    for (const ExprPtr& arg : expr.args) {
      Info tmp     = this->eval(arg);
      info.history = std::max(info.history, tmp.history);
      info.horizon = std::max(info.horizon, tmp.horizon);
    }
    return info;
  }

  Info operator()(const SpatioTemporalOp& expr) {
    Info info     = {0, 0};
    auto interval = (expr.interval != nullptr) ? this->eval(*expr.interval)
                                               : utils::UintOrInf::infinity();
    switch (expr.op) {
      case SpatioTemporalOp::Type::Next:
        info = this->eval(expr.args.back());
        info.horizon += 1;
        return info;
      case SpatioTemporalOp::Type::Previous:
        info = this->eval(expr.args.back());
        info.history += 1;
        return info;
      case SpatioTemporalOp::Type::Eventually:
      case SpatioTemporalOp::Type::Always:
        info = this->eval(expr.args.back());
        info.horizon += interval;
        return info;
      case SpatioTemporalOp::Type::Once:
      case SpatioTemporalOp::Type::Historically:
        info = this->eval(expr.args.back());
        info.history += interval;
        return info;
      case SpatioTemporalOp::Type::Until:
      case SpatioTemporalOp::Type::Since:
        // Handle separately
        break;
    }
    // We know that it is either Since or Until.
    // The hist and hrz equations for the two are identical, but flipped. So let's just
    // do that.
    Info left  = this->eval(expr.args[0]);
    Info right = this->eval(expr.args[1]);
    // Assume it is Until
    info.horizon = std::max(left.horizon + interval - 1, right.horizon + interval);
    info.history = std::max(left.history, right.history);
    // If it is Since, swap
    if (expr.op == SpatioTemporalOp::Type::Since) {
      std::swap(info.history, info.horizon);
    }
    return info;
  }

  Info operator()(const SpatialQuantifier& expr) {
    return this->eval(expr.arg);
  }
};

} // namespace

namespace percemon {

inline namespace monitor_info {
MonitorInfo MonitorInfo::get(const ExprPtr& expr, double fps) {
  if (expr == nullptr) {
    throw std::invalid_argument("Nullptr cannot have an horizon");
  }
  utils::assert_(expr->id() != 0, "Hash shouldn't be 0");

  HorizonCompute::Info info = utils::visit(HorizonCompute{fps}, *expr);
  MonitorInfo ret;
  if (info.horizon.is_finite()) {
    ret.horizon = info.horizon.value();
  } else {
    ret.horizon = {};
  }
  if (info.history.is_finite()) {
    ret.history = info.history.value();
  } else {
    ret.history = {};
  }
  return ret;
}

} // namespace monitor_info
} // namespace percemon
