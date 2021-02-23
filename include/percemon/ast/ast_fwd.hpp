/// @file   ast/ast_fwd.hpp
/// @brief  Forward declaration of AST stuff

#pragma once
#ifndef PERCEMON_AST_FWDDECL
#define PERCEMON_AST_FWDDECL

#include <memory>

#define PERCEMON_AST_NS percemon::ast::details

namespace percemon {
struct Expr; // IWYU pragma: keep
using ExprPtr = std::shared_ptr<Expr>;
} // namespace percemon

#endif /* end of include guard: PERCEMON_AST_FWDDECL */
