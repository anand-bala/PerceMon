#include "percemon/algorithms/default_monitor.hpp"
#include "percemon/algorithms.hpp"
#include "percemon/ast.hpp"
#include "percemon/topo.hpp"

#include "utils/static_analysis_helpers.hpp"
#include "utils/visit.hpp"

#include <range/v3/view.hpp>
#include <range/v3/view/cartesian_product.hpp>

#include <stdexcept>
#include <type_traits>
#include <variant>

namespace percemon {
inline namespace monitors {

using namespace ast::details;

static constexpr bool check_returns_topology(const Expr& expr) {
  if (const auto ptr = std::get_if<SpatialOp>(&expr)) {
    return ptr->op != SpatialOp::Type::Area; // Everything except Area returns topology.
  }
  return std::holds_alternative<SpatioTemporalOp>(expr);
}

static constexpr bool check_returns_rob(const Expr& expr) {
  return !check_returns_topology(expr);
}

struct Monitor::Impl {
  /// Formula to be monitored
  ExprPtr phi;

  /// Bounding box for the frame.
  topo::BoundingBox bounds;

  /// Frames per second for the datastream
  double fps;
  /// History required for the formula.
  std::optional<size_t> history_size;
  /// Future frames required for the formula.
  std::optional<size_t> horizon_size;

  size_t max_buf_size;
  /// Need to check the history or horizon before using this.
  size_t now_offset;

  /// Check if the buffer is dirty, i.e., we haven't computed the robustness for the
  /// current state of the buffer.
  bool is_dirty;

  /// A buffer containing the history of Frames required to compute robustness of phi
  /// efficiently.
  std::deque<datastream::Frame> buffer;

  std::map<size_t, std::deque<double>> rob_table;
  std::map<size_t, std::deque<topo::TopologyPtr>> reg_table;

  /// Maintain a map from Var_x and Var_f to size_t (double gets converted using fps)
  std::map<std::string, double> var_map;

  /// Maintain a map from Var_id to an actual object ID in frame
  std::map<std::string, std::string> obj_map;

  Impl(ExprPtr phi_, topo::BoundingBox bbox, double fps_) :
      phi{std::move(phi_)},
      bounds{bbox},
      fps{fps_},
      max_buf_size{1},
      now_offset{0},
      is_dirty{false} {
    if (phi == nullptr) {
      throw std::invalid_argument("Cannot monitor a nullptr expression");
    } else {
      // Check if m_phi is a top-level operation
      bool is_ok = utils::visit(
          utils::overloaded{
              [](const Constant& c) { return c.is_bool(); },
              [](const Variable&) { return false; },
              [](const ArithmeticFn&) { return false; },
              [](const SpatialOp&) { return false; },
              [](const SpatioTemporalOp&) { return false; },
              [](const auto&) { return true; },
          },
          *phi);
      if (!is_ok) {
        throw std::invalid_argument(
            "Given expression is not monitorable as it is an invalid top level operation");
      }
    }
    if (fps <= 0) {
      throw std::invalid_argument("FPS cannot be <= 0");
    }

    auto monitor_info = MonitorInfo::get(phi, fps);
    history_size      = monitor_info.history;
    horizon_size      = monitor_info.horizon;
    if (history_size.has_value() && history_size.has_value()) {
      // Both, history and horizon are bounded
      max_buf_size =
          *history_size + *horizon_size + 1; // May need to check for integer overflow.
      // Set the index to "0"
      now_offset = *history_size;
    } else { // Buffer is unbounded. Must be offline monitoring.
      max_buf_size = static_cast<size_t>(-1);
      if (history_size.has_value()) {
        // Unbounded horizon, but bounded history.
        now_offset = *history_size;
      } else if (horizon_size.has_value()) {
        // Unbounded history, but bounded horizon
        // Offset will be from the back.
        now_offset = *horizon_size + 1;
      } else {
        throw std::invalid_argument(
            "PerceMon doesn't support unbounded history _and_ horizon together");
      }
    }
  }

  /// Get the index for the current frame being processed
  [[nodiscard]] constexpr size_t now_buf_idx() const {
    /// History & Horizon can't both be unbounded (by construction).
    if (history_size.has_value() && horizon_size.has_value()) {
      // given value is offset from front.
      return now_offset;
    } else { // buffer is unbounded.
      // Check if it is underfull.
      // TODO: Underfull is stupid and should never happpen.
      // XXX: Undefined behavior
      const bool is_underfull = now_offset > buffer.size();
      if (history_size.has_value()) { // Bounded history, unbounded horizon
        return is_underfull ? buffer.size() - 1 : now_offset;
      } else if (horizon_size.has_value()) { // Bounded horizon, unbounded history
        return is_underfull ? 0 : buffer.size() - now_offset;
      } else {
        utils::unreachable(
            "Constructor prevents from both, history and horizon being unbounded");
      }
    }
  }

  [[nodiscard]] double get_current_rob() const {
    return rob_table.at(phi->id()).at(now_buf_idx());
  }

  void eval(const ExprPtr& expr) {
    utils::assert_(expr != nullptr, "Can't evaluate null Expr");
    if (!is_dirty) {
      // Do nothing.
      return;
    }
    utils::visit(*this, *expr, expr->id());
    /// Shorten the deque if they are too large
    if (check_returns_rob(*expr)) {
      auto& row = rob_table[expr->id()];
      while (row.size() > max_buf_size) { row.pop_front(); }
      utils::assert_(buffer.size() == row.size());
    } else { // region
      auto& row = reg_table[expr->id()];
      while (row.size() > max_buf_size) { row.pop_front(); }
      utils::assert_(buffer.size() == row.size());
    }
  }

  /// Just add the scalar value to the back of the row
  void operator()(const Constant& expr, const size_t id) {
    const double val = utils::visit(
        utils::overloaded{
            [this](const C_TIME) { return this->buffer.back().timestamp.seconds(); },
            [this](const C_FRAME) {
              return static_cast<double>(this->buffer.back().frame_num);
            },
            [](const auto c) { return 1.0 * c; },
        },
        expr);
    rob_table[id].push_back(val);
  }

  /// Change the entire row to the same value as Frame/Time Vars, ignore Obj vars, and
  /// other stuff shouldn't happen.
  void operator()(const Variable& expr, const size_t id) {
    double val = std::numeric_limits<double>::infinity();
    switch (expr.type) {
      case Variable::Type::Frame:
      case Variable::Type::Timepoint:
        val = var_map.at(expr.name);
        break;
      case Variable::Type::Object:
        // Handled directly in functions that reference this.
        return;
      default:
        utils::unreachable("This shouldn't happen right now");
    }

    rob_table[id] = std::deque<double>(buffer.size(), val);
  }

  void operator()(const ArithmeticFn& expr, const size_t id) {}
  void operator()(const PredicateOp& expr, const size_t id) {}
  void operator()(const LogicalOp& expr, const size_t id) {}
  void operator()(const QuantifierOp& expr, const size_t id) {}
  void operator()(const PinnedFrame& expr, const size_t id) {}
  void operator()(const TemporalOp& expr, const size_t id) {}
  void operator()(const SpatialOp& expr, const size_t id) {}
  void operator()(const SpatioTemporalOp& expr, const size_t id) {}
  void operator()(const SpatialQuantifier& expr, const size_t id) {}
};

Monitor::Monitor(ExprPtr phi, topo::BoundingBox frame_size, double fps) :
    m_impl{std::make_unique<Impl>(std::move(phi), frame_size, fps)} {}

void Monitor::add_frame(const datastream::Frame& frame) {
  /// Add frame to buffer
  m_impl->buffer.push_back(frame);
  // Resize buffer while popping from front. Should only happen once.
  while (m_impl->buffer.size() > m_impl->max_buf_size) { m_impl->buffer.pop_front(); }
  // Mark as dirty
  m_impl->is_dirty = true;
}

void Monitor::add_frame(datastream::Frame&& frame) {
  /// Add frame to buffer
  m_impl->buffer.push_back(frame);
  // Resize buffer while popping from front. Should only happen once.
  while (m_impl->buffer.size() > m_impl->max_buf_size) { m_impl->buffer.pop_front(); }
  // Mark as dirty
  m_impl->is_dirty = true;
}

double Monitor::eval() {
  m_impl->eval(m_impl->phi);
  m_impl->is_dirty = false;
  return m_impl->get_current_rob();
}

} // namespace monitors

} // namespace percemon
