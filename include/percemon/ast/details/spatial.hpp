/// @file     ast/details/spatial.hpp
/// @brief    Generic AST nodes for spatial and spatio-tempoal operations
///
/// Here, we define the set of operations defined by the MTL+S4U spatio-temporal logic.
#pragma once
#ifndef PERCEMON_AST_DETAILS_SPATIAL
#define PERCEMON_AST_DETAILS_SPATIAL

#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "percemon/ast/ast_fwd.hpp"

namespace percemon::ast::details {

/// @brief Functions on Topologies that return other Topologies.
///
/// Essentially, this node encodes all operations that can be used on topological
/// objects, like Complement, Intersect, Union, etc.
///
/// @note
/// Arguments here must be other spatial operations. For the special case of BBox, the
/// argument must be an Object ID variable.
struct SpatialOp {
  enum struct Type { BBox, Area, Complement, Interior, Closure, Intersect, Union };

  Type op;
  std::vector<ExprPtr> args;

  SpatialOp(Type operation, std::vector<ExprPtr> arguments);

  [[nodiscard]] std::string to_string() const;
};

/// @brief Spatio-temporal operations
///
/// @note
/// The argument to this must be either SpatialOp or SpatioTemporalOp
struct SpatioTemporalOp {
  enum struct Type {
    Next,
    Previous,
    Eventually,
    Once,
    Always,
    Historically,
    Until,
    Since,
  };

  Type op;
  std::vector<ExprPtr> args; // Has max 2 arguments.
  std::shared_ptr<Interval> interval;

  SpatioTemporalOp(
      Type operation,
      std::vector<ExprPtr> arguments,
      std::shared_ptr<Interval> interval_arg);

  SpatioTemporalOp(Type operation, std::vector<ExprPtr> arguments) :
      SpatioTemporalOp{operation, std::move(arguments), {}} {}

  [[nodiscard]] std::string to_string() const;
};

/// @brief Spatial quantifiers.
///
/// These are the smallest spatial operations that can be used in the standard
/// Temporal/Propositional logic.
///
/// @note
/// The argument to this must be either SpatialOp or SpatioTemporalOp
struct SpatialQuantifier {
  // I retain old names for sanity.
  // Should probably change these to NotEmpty and Fills.
  enum struct Type { Exists, Forall };

  Type op;
  ExprPtr arg;

  SpatialQuantifier(Type operation, ExprPtr argument);

  [[nodiscard]] std::string to_string() const;
};

} // namespace percemon::ast::details

#endif /* end of include guard: PERCEMON_AST_DETAILS_SPATIAL */
