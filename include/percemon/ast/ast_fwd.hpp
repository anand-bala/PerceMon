/// @file   ast/ast_fwd.hpp
/// @brief  Forward declaration of AST stuff

#pragma once
#ifndef PERCEMON_AST_FWDDECL
#define PERCEMON_AST_FWDDECL

#include <memory>
#include <variant>

namespace percemon {
struct Expr; // IWYU pragma: keep
using ExprPtr = std::shared_ptr<Expr>;

namespace ast::details {

struct C_TIME;
struct C_FRAME;
struct Constant;
struct Variable;

struct ArithmeticFn;
struct PinnedFrame;
struct Interval;

struct PredicateOp;
struct LogicalOp;
struct QuantifierOp;

struct TemporalOp;

struct SpatialOp;
struct SpatialQuantifier;
struct SpatioTemporalOp;

struct Attribute;
} // namespace ast::details

} // namespace percemon

#endif /* end of include guard: PERCEMON_AST_FWDDECL */
