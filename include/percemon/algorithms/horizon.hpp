/// @file     percemon/algorithms/horizon.hpp
/// @brief    Compute the horizon required to monitor a formula.

#pragma once
#ifndef PERCEMON_ALGORITHMS_HORIZON
#define PERCEMON_ALGORITHMS_HORIZON

#include "percemon/ast/ast_fwd.hpp"

#include <optional>

namespace percemon {

inline namespace horizon {

/// @brief Compute the number of frames required to monitor the given formula.
///
/// @return An optional result, which will contain a value if the horizon is bounded.
///         Otherwise nothing.
std::optional<size_t> compute_horizon(const ExprPtr&);

} // namespace horizon

} // namespace percemon

#endif /* end of include guard: PERCEMON_ALGORITHMS_HORIZON */
