#pragma once

#ifndef __PERCEMON_AST_FUNCTIONS_HPP__
#define __PERCEMON_AST_FUNCTIONS_HPP__

#include "percemon/ast/primitives.hpp"

namespace percemon::ast::functions {

using namespace primitives;

// Primitive operations on Time/Frames

struct TimeInterval {
  double low, high;

  enum Bound { OPEN, LOPEN, ROPEN, CLOSED };
  Bound bound;

  static TimeInterval open(double low, double high) {
    return TimeInterval{low, high, OPEN};
  }
  static TimeInterval lopen(double low, double high) {
    return TimeInterval{low, high, LOPEN};
  }
  static TimeInterval ropen(double low, double high) {
    return TimeInterval{low, high, ROPEN};
  }
  static TimeInterval closed(double low, double high) {
    return TimeInterval{low, high, CLOSED};
  }
};

struct FrameInterval {
  size_t low, high;

  enum Bound { OPEN, LOPEN, ROPEN, CLOSED };
  Bound bound;

  static FrameInterval open(size_t low, size_t high) {
    return FrameInterval{low, high, OPEN};
  }
  static FrameInterval lopen(size_t low, size_t high) {
    return FrameInterval{low, high, LOPEN};
  }
  static FrameInterval ropen(size_t low, size_t high) {
    return FrameInterval{low, high, ROPEN};
  }
  static FrameInterval closed(size_t low, size_t high) {
    return FrameInterval{low, high, CLOSED};
  }
};

/**
 * A bound on a Var_x of the form
 *
 *    x - C_TIME ~ c
 *
 * where, `~` is a relational operator: `<`,`>`,`<=`,`>=`.
 */
struct TimeBound {
  Var_x x;
  ComparisonOp op = ComparisonOp::GE;
  double bound    = 0.0;

  TimeBound() = delete;
  TimeBound(
      Var_x x_,
      const ComparisonOp op_ = ComparisonOp::GE,
      const double bound_    = 0.0) :
      x{std::move(x_)}, op{op_}, bound{bound_} {
    if (op == ComparisonOp::EQ || op == ComparisonOp::NE) {
      throw std::invalid_argument(
          "TimeBound (Var_x - C_TIME ~ c) cannot have == and != constraints.");
    }
    if (bound_ <= 0.0) {
      bound = -bound_;
      switch (op_) {
        case ComparisonOp::GE: op = ComparisonOp::LE; break;
        case ComparisonOp::GT: op = ComparisonOp::LT; break;
        case ComparisonOp::LE: op = ComparisonOp::GE; break;
        case ComparisonOp::LT: op = ComparisonOp::GT; break;
        default: {
          throw std::invalid_argument(
              "TimeBound (Var_x - C_TIME ~ c) cannot have == and != constraints.");
        }
      }
    }
  };

  inline bool operator==(const TimeBound& other) const {
    return (x == other.x) && (op == other.op) && (bound == other.bound);
  };

  inline bool operator!=(const TimeBound& other) const { return !(*this == other); };
};
TimeBound operator-(const Var_x& lhs, C_TIME);
TimeBound operator>(const TimeBound& lhs, const double bound);
TimeBound operator>=(const TimeBound& lhs, const double bound);
TimeBound operator<(const TimeBound& lhs, const double bound);
TimeBound operator<=(const TimeBound& lhs, const double bound);
TimeBound operator>(const double bound, const TimeBound& rhs);
TimeBound operator>=(const double bound, const TimeBound& rhs);
TimeBound operator<(const double bound, const TimeBound& rhs);
TimeBound operator<=(const double bound, const TimeBound& rhs);

/**
 * A bound on a Var_f of the form
 *
 *    f - C_FRAME ~ c
 *
 * where, `~` is a relational operator: `<`,`>`,`<=`,`>=`.
 */
struct FrameBound {
  Var_f f;
  ComparisonOp op = ComparisonOp::GE;
  size_t bound    = 0;

  FrameBound() = delete;
  FrameBound(
      Var_f f_,
      const ComparisonOp op_ = ComparisonOp::GE,
      const size_t bound_    = 0) :
      f{std::move(f_)}, op{op_}, bound{bound_} {
    if (op == ComparisonOp::EQ || op == ComparisonOp::NE) {
      throw std::invalid_argument(
          "FrameBound (Var_f - C_FRAME ~ c) cannot have == and != constraints.");
    }
  };

  inline bool operator==(const FrameBound& other) const {
    return (f == other.f) && (op == other.op) && (bound == other.bound);
  };

  inline bool operator!=(const FrameBound& other) const { return !(*this == other); };
};
FrameBound operator-(const Var_f& lhs, C_FRAME);
FrameBound operator>(const FrameBound& lhs, const size_t bound);
FrameBound operator>=(const FrameBound& lhs, const size_t bound);
FrameBound operator<(const FrameBound& lhs, const size_t bound);
FrameBound operator<=(const FrameBound& lhs, const size_t bound);
FrameBound operator>(const size_t bound, const FrameBound& rhs);
FrameBound operator>=(const size_t bound, const FrameBound& rhs);
FrameBound operator<(const size_t bound, const FrameBound& rhs);
FrameBound operator<=(const size_t bound, const FrameBound& rhs);

using TemporalBoundExpr = std::variant<TimeBound, FrameBound>;

// Some primitive operations on IDs

/**
 * Node comparing objects.
 */
struct CompareId {
  Var_id lhs;
  ComparisonOp op;
  Var_id rhs;

  CompareId() = delete;
  CompareId(Var_id lhs_, ComparisonOp op_, Var_id rhs_) :
      lhs{std::move(lhs_)}, op{op_}, rhs{std::move(rhs_)} {
    if (op != ComparisonOp::EQ && op != ComparisonOp::NE) {
      throw std::invalid_argument(
          "Cannot use relational operators <, >, <=, >= to compare Var_id");
    }
  };
};
CompareId operator==(const Var_id& lhs, const Var_id& rhs);
CompareId operator!=(const Var_id& lhs, const Var_id& rhs);

/**
 * Node to represent the Class(id) function.
 */
struct Class {
  Var_id id;

  Class() = delete;
  Class(Var_id id_) : id{std::move(id_)} {}
};

/**
 * Node to compare equality of object class between either two IDs or an ID and a
 * class literal.
 */
struct CompareClass {
  Class lhs;
  ComparisonOp op;
  std::variant<int, Class> rhs;

  CompareClass() = delete;
  CompareClass(Class lhs_, ComparisonOp op_, std::variant<int, Class> rhs_) :
      lhs{std::move(lhs_)}, op{op_}, rhs{std::move(rhs_)} {
    if (op != ComparisonOp::EQ && op != ComparisonOp::NE) {
      throw std::invalid_argument(
          "Cannot use relational operators <, >, <=, >= to compare Class(id)");
    }
  };
};
CompareClass operator==(const Class& lhs, const int rhs);
CompareClass operator==(const int lhs, const Class& rhs);
CompareClass operator==(const Class& lhs, const Class& rhs);
CompareClass operator!=(const Class& lhs, const int rhs);
CompareClass operator!=(const int lhs, const Class& rhs);
CompareClass operator!=(const Class& lhs, const Class& rhs);

/**
 * Node representing the Prob(id) function. Providing a multiplier is equivalent to
 * Prob(id) * scale.
 */
struct Prob {
  Var_id id;
  double scale = 1.0;

  Prob() = delete;
  Prob(Var_id id_, double scale_ = 1.0) : id{std::move(id_)}, scale{scale_} {}

  Prob& operator*=(const double rhs) {
    this->scale *= rhs;
    return *this;
  };
  friend Prob operator*(const Prob& lhs, const double rhs) {
    return Prob{lhs.id, lhs.scale * rhs};
  }

  friend Prob operator*(const double lhs, const Prob& rhs) { return rhs * lhs; }
};

struct CompareProb {
  Prob lhs;
  ComparisonOp op;
  std::variant<double, Prob> rhs;

  CompareProb() = delete;
  CompareProb(Prob lhs_, ComparisonOp op_, std::variant<double, Prob> rhs_) :
      lhs{std::move(lhs_)}, op{op_}, rhs{std::move(rhs_)} {
    if (op == ComparisonOp::EQ || op == ComparisonOp::NE) {
      throw std::invalid_argument(
          "Cannot use relational operators ==, != to compare Prob(id)");
    }
  };
};
CompareProb operator>(const Prob& lhs, const double rhs);
CompareProb operator>=(const Prob& lhs, const double rhs);
CompareProb operator<(const Prob& lhs, const double rhs);
CompareProb operator<=(const Prob& lhs, const double rhs);
CompareProb operator>(const Prob& lhs, const Prob& rhs);
CompareProb operator>=(const Prob& lhs, const Prob& rhs);
CompareProb operator<(const Prob& lhs, const Prob& rhs);
CompareProb operator<=(const Prob& lhs, const Prob& rhs);

// Spatial primitive operations
struct BBox {
  Var_id id;
};

struct ED {
  Var_id id1;
  CRT crt1;
  Var_id id2;
  CRT crt2;
  double scale = 1.0;

  ED() = delete;
  ED(Var_id id1_, CRT crt1_, Var_id id2_, CRT crt2_, double scale_ = 1.0) :
      id1{std::move(id1_)},
      crt1{crt1_},
      id2{std::move(id2_)},
      crt2{crt2_},
      scale{scale_} {}

  ED& operator*=(const double rhs) {
    this->scale *= rhs;
    return *this;
  };
  friend ED operator*(const ED& lhs, const double rhs) {
    return ED{lhs.id1, lhs.crt1, lhs.id2, lhs.crt2, lhs.scale * rhs};
  }

  friend ED operator*(const double lhs, const ED& rhs) { return rhs * lhs; }
};

struct CompareED {
  ED lhs;
  ComparisonOp op;
  double rhs;

  CompareED() = delete;
  CompareED(ED lhs_, ComparisonOp op_, double rhs_) :
      lhs{std::move(lhs_)}, op{op_}, rhs{rhs_} {
    if (op == ComparisonOp::EQ || op == ComparisonOp::NE) {
      throw std::invalid_argument(
          "Cannot use relational operators ==, != to compare Euclidean Distance");
    }
  };
};

CompareED operator>(const ED& lhs, const double rhs);
CompareED operator>=(const ED& lhs, const double rhs);
CompareED operator<(const ED& lhs, const double rhs);
CompareED operator<=(const ED& lhs, const double rhs);
CompareED operator>(const double lhs, const ED& rhs);
CompareED operator>=(const double lhs, const ED& rhs);
CompareED operator<(const double lhs, const ED& rhs);
CompareED operator<=(const double lhs, const ED& rhs);
CompareED operator>(const ED& lhs, const ED& rhs);
CompareED operator>=(const ED& lhs, const ED& rhs);
CompareED operator<(const ED& lhs, const ED& rhs);
CompareED operator<=(const ED& lhs, const ED& rhs);

struct Lat {
  Var_id id;
  CRT crt;
  double scale = 1.0;

  Lat() = delete;
  Lat(Var_id id_, CRT crt_, double scale_ = 1.0) :
      id{std::move(id_)}, crt{crt_}, scale{scale_} {}

  Lat& operator*=(const double rhs) {
    this->scale *= rhs;
    return *this;
  };
  friend Lat operator*(Lat lhs, const double rhs) {
    lhs *= rhs;
    return lhs;
  }

  friend Lat operator*(const double lhs, const Lat& rhs) { return rhs * lhs; }
};

struct Lon {
  Var_id id;
  CRT crt;
  double scale = 1.0;

  Lon() = delete;
  Lon(Var_id id_, CRT crt_, double scale_ = 1.0) :
      id{std::move(id_)}, crt{crt_}, scale{scale_} {}

  Lon& operator*=(const double rhs) {
    this->scale *= rhs;
    return *this;
  };
  friend Lon operator*(Lon lhs, const double rhs) {
    lhs *= rhs;
    return lhs;
  }

  friend Lon operator*(const double lhs, const Lon& rhs) { return rhs * lhs; }
};

struct CompareLat {
  Lat lhs;
  ComparisonOp op;
  std::variant<double, Lat, Lon> rhs;

  CompareLat() = delete;
  CompareLat(Lat lhs_, ComparisonOp op_, std::variant<double, Lat, Lon> rhs_) :
      lhs{std::move(lhs_)}, op{op_}, rhs{std::move(rhs_)} {
    if (op == ComparisonOp::EQ || op == ComparisonOp::NE) {
      throw std::invalid_argument(
          "Cannot use relational operators ==, != to compare Lat(id)");
    }
  };
};

struct CompareLon {
  Lon lhs;
  ComparisonOp op;
  std::variant<double, Lat, Lon> rhs;

  CompareLon() = delete;
  CompareLon(Lon lhs_, ComparisonOp op_, std::variant<double, Lat, Lon> rhs_) :
      lhs{std::move(lhs_)}, op{op_}, rhs{std::move(rhs_)} {
    if (op == ComparisonOp::EQ || op == ComparisonOp::NE) {
      throw std::invalid_argument(
          "Cannot use relational operators ==, != to compare Lon(id)");
    }
  };
};

CompareLat operator>(const double lhs, const Lat& rhs);
CompareLat operator>=(const double lhs, const Lat& rhs);
CompareLat operator<(const double lhs, const Lat& rhs);
CompareLat operator<=(const double lhs, const Lat& rhs);
CompareLat operator>(const Lat& lhs, const double rhs);
CompareLat operator>=(const Lat& lhs, const double rhs);
CompareLat operator<(const Lat& lhs, const double rhs);
CompareLat operator<=(const Lat& lhs, const double rhs);
CompareLat operator>(const Lat& lhs, const Lat& rhs);
CompareLat operator>=(const Lat& lhs, const Lat& rhs);
CompareLat operator<(const Lat& lhs, const Lat& rhs);
CompareLat operator<=(const Lat& lhs, const Lat& rhs);
CompareLat operator>(const Lat& lhs, const Lon& rhs);
CompareLat operator>=(const Lat& lhs, const Lon& rhs);
CompareLat operator<(const Lat& lhs, const Lon& rhs);
CompareLat operator<=(const Lat& lhs, const Lon& rhs);

CompareLon operator>(const double lhs, const Lon& rhs);
CompareLon operator>=(const double lhs, const Lon& rhs);
CompareLon operator<(const double lhs, const Lon& rhs);
CompareLon operator<=(const double lhs, const Lon& rhs);
CompareLon operator>(const Lon& lhs, const double rhs);
CompareLon operator>=(const Lon& lhs, const double rhs);
CompareLon operator<(const Lon& lhs, const double rhs);
CompareLon operator<=(const Lon& lhs, const double rhs);
CompareLon operator>(const Lon& lhs, const Lon& rhs);
CompareLon operator>=(const Lon& lhs, const Lon& rhs);
CompareLon operator<(const Lon& lhs, const Lon& rhs);
CompareLon operator<=(const Lon& lhs, const Lon& rhs);
CompareLon operator>(const Lon& lhs, const Lat& rhs);
CompareLon operator>=(const Lon& lhs, const Lat& rhs);
CompareLon operator<(const Lon& lhs, const Lat& rhs);
CompareLon operator<=(const Lon& lhs, const Lat& rhs);

struct AreaOf {
  Var_id id;
  double scale = 1.0;

  AreaOf() = delete;
  AreaOf(Var_id id_, double scale_ = 1.0) : id{std::move(id_)}, scale{scale_} {}

  AreaOf& operator*=(const double rhs) {
    this->scale *= rhs;
    return *this;
  };
  friend AreaOf operator*(const AreaOf& lhs, const double rhs) {
    return AreaOf{lhs.id, lhs.scale * rhs};
  }

  friend AreaOf operator*(const double lhs, const AreaOf& rhs) { return rhs * lhs; }
};

struct CompareArea {
  AreaOf lhs;
  ComparisonOp op;
  std::variant<double, AreaOf> rhs;

  CompareArea() = delete;
  CompareArea(AreaOf lhs_, ComparisonOp op_, std::variant<double, AreaOf> rhs_) :
      lhs{std::move(lhs_)}, op{op_}, rhs{std::move(rhs_)} {
    if (op == ComparisonOp::EQ || op == ComparisonOp::NE) {
      throw std::invalid_argument(
          "Cannot use relational operators ==, != to compare AreaOf(id)");
    }
  };
};
CompareArea operator>(const AreaOf& lhs, const double rhs);
CompareArea operator>=(const AreaOf& lhs, const double rhs);
CompareArea operator<(const AreaOf& lhs, const double rhs);
CompareArea operator<=(const AreaOf& lhs, const double rhs);
CompareArea operator>(const double lhs, const AreaOf& rhs);
CompareArea operator>=(const double lhs, const AreaOf& rhs);
CompareArea operator<(const double lhs, const AreaOf& rhs);
CompareArea operator<=(const double lhs, const AreaOf& rhs);
CompareArea operator>(const AreaOf& lhs, const AreaOf& rhs);
CompareArea operator>=(const AreaOf& lhs, const AreaOf& rhs);
CompareArea operator<(const AreaOf& lhs, const AreaOf& rhs);
CompareArea operator<=(const AreaOf& lhs, const AreaOf& rhs);

} // namespace percemon::ast::functions

#endif /* end of include guard: __PERCEMON_AST_FUNCTIONS_HPP__ */
