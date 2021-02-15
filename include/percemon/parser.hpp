/// @file   PerceMon/parser.hpp
/// @brief  Interface to the parser for the PerceMon specification language.
///
/// Here, we define the general interface for parsing the PerceMon specification language
/// from a string and from a file into a context.

#pragma once

#ifndef PERCEMON_PARSER_HPP
#define PERCEMON_PARSER_HPP

#include <cstddef>
#include <map>
#include <memory>
#include <string>
#include <string_view>

#include "percemon/internal/filesystem.hpp"

namespace percemon {

// Forward Declaration for Expr (holds valid expressions).
struct Expr;

using ExprPtr = std::shared_ptr<Expr>;

// Forward Declaration for MonitorConfig (holds necessary information to build monitors
// for different formulas).
struct MonitorConfig;

using MonitorConfigPtr = std::shared_ptr<MonitorConfig>;

/// The type of syntax that can be used in the specification.
///
/// - `FUTURE`: Implies that only future-time temporal operators (`Next`, `Always`,
///   `Eventually`, and `Until`) can be used in the specification.
/// - `PAST`: Implies that only _past-time_ temporal operators (`Prev`, `Historically`,
///   `Once`, and `Until`) can be used.
/// - `MIXED`: Implies that a mix of past-time and future-time operators can be used in
///   the specification language.
///
/// @note
/// Once the syntax has been set in the specification, we also need to check if the
/// chosen logic and the semantics support the given syntax.
enum struct SyntaxSettings { FUTURE, PAST, MIXED };

/// Holds the context of the parsed specification.
struct Context {
 public:
  Context() = default;

  /// Given a `string_view` of the actual specification (typically read from the
  /// specification script file), this fuction will return the parsed contents.
  ///
  /// Note that by returning a `std::unique_ptr`, the `signal_tl` library gives
  /// up ownership of the `Context`. The user of the library can manipulate
  /// the `Context` struct however they like, but the specification may
  /// lose its meaning once you do.
  static std::unique_ptr<Context> from_string(std::string_view);

  /// Given a `std::filesystem::path` to the specification file, this function
  /// reads the file and creates a concrete `Context` from it.
  ///
  /// Note that by returning a `std::unique_ptr`, the `signal_tl` library gives
  /// up ownership of the `Context`. The user of the library can manipulate
  /// the `Context` struct however they like, but the specification may
  /// lose its meaning once you do.
  static std::unique_ptr<Context> from_file(const fs::path&);

  /// Syntax settings.
  SyntaxSettings syntax_settings = SyntaxSettings::MIXED;
  /// List of defined formulas, keyed by their corresponding identifiers.
  std::map<std::string, ExprPtr> defined_formulas;
  /// List of settings for monitors, keyed by their corresponding identifiers.
  std::map<std::string, ExprPtr> monitors;
};

// LCOV_EXCL_START
namespace grammar::internal {

/// **INTERNAL USE ONLY**
///
/// This is used to call `tao::pagtl::contrib::analyze`, a function that
/// analyzes the parser grammar for construction errors like unresolved cycles,
/// etc. Used in the tests to check the grammar and is useful only for
/// developers of this library.
size_t analyze(int verbose = 1);

bool trace_from_file(const fs::path&);

} // namespace grammar::internal

// LCOV_EXCL_STOP

} // namespace percemon

#endif /* end of include guard: PERCEMON_PARSER_HPP */
