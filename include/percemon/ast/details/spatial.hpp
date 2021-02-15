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

// Forward-declare Expr
namespace percemon {
struct Expr;
} // namespace percemon

namespace percemon::ast::details {

/// @brief Functions on Topologies that return other Topologies.
///
/// Essentially, this node encodes all operations that can be used on topological
/// objects, like Complement, Intersect, Union, etc.
struct SpatialOp {
  enum struct Type { Complement, Intersect, Union, Interior, Closure };

  Type op;
  std::vector<std::shared_ptr<Expr>> args;
};

/// @brief Spatial quantifiers.
///
/// These are the smallest spatial operations that can be used in the standard
/// Temporal/Propositional logic.
struct SpatialQuantifier {
  // I retain old names for sanity.
  // Should probably change these to NotEmpty and Fills.
  enum struct Type { SpExists, SpForall };

  Type op;
  std::shared_ptr<Expr> arg;
};

/// @brief Spatio-temporal operations
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
  std::shared_ptr<Expr> interval;
  std::array<std::shared_ptr<Expr>, 2> args; // Has max 2 arguments.
};

} // namespace percemon::ast::details

#endif /* end of include guard: PERCEMON_AST_DETAILS_SPATIAL */
