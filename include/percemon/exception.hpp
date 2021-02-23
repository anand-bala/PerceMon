#pragma once

#ifndef __PERCEMON_EXCEPTION_HH__
#define __PERCEMON_EXCEPTION_HH__

#include <exception>
#include <string>

namespace percemon {

struct not_implemented_error : public std::exception {
 private:
  const std::string m_what_arg;

 public:
  not_implemented_error(std::string what_arg) : m_what_arg{std::move(what_arg)} {}
  not_implemented_error(const char* what_arg) : m_what_arg{what_arg} {}

  not_implemented_error(const not_implemented_error& other) noexcept = default;

  [[nodiscard]] const char* what() const noexcept override {
    return m_what_arg.c_str();
  }
};

} // namespace percemon

#endif /* end of include guard: __PERCEMON_EXCEPTION_HH__ */
