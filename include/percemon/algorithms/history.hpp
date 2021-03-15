/// @file     percemon/algorithms/history.hpp
/// @brief    Compute the history required to monitor a formula.

#pragma once
#ifndef PERCEMON_ALGORITHMS_HISTORY
#define PERCEMON_ALGORITHMS_HISTORY

#include "percemon/ast/ast_fwd.hpp"

#include <optional>

namespace percemon {

inline namespace history {

/// @brief Compute the number of frames required to monitor the given formula.
///
/// @return An optional result, which will contain a value if the history is bounded.
///         Otherwise nothing.
std::optional<size_t> compute_history(const ExprPtr&);

} // namespace history

} // namespace percemon

#endif /* end of include guard: PERCEMON_ALGORITHMS_HISTORY */
