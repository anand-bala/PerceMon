set(PERCEMON_VERSION_MAJOR 0)
set(PERCEMON_VERSION_MINOR 1)
set(PERCEMON_VERSION_PATCH 0)
set(PERCEMON_VERSION
    "${PERCEMON_VERSION_MAJOR}.${PERCEMON_VERSION_MINOR}.${PERCEMON_VERSION_PATCH}"
    CACHE STRING "percemon library version")

set(PERCEMON_FULL_VERSION
    "${PERCEMON_VERSION_MAJOR}.${PERCEMON_VERSION_MINOR}.${PERCEMON_VERSION_PATCH}-dev"
    CACHE STRING "percemon library version (with pre-release metadata)")

message(CHECK_START "Finding working git")
find_program(GIT_EXE git REQUIRED)
if(NOT GIT_EXE)
  message(CHECK_FAIL "not found")
  return()
endif()

message(CHECK_PASS "found ${GIT_EXE}")
execute_process(
  COMMAND ${GIT_EXE} -C ${CMAKE_CURRENT_SOURCE_DIR} describe --match *.*.*
          --tags
  RESULT_VARIABLE EXIT_STATUS
  OUTPUT_VARIABLE GIT_DESCRIBE
  OUTPUT_STRIP_TRAILING_WHITESPACE)
if(EXIT_STATUS EQUAL "0")
  message(STATUS "Attempting to parse for tagged release version.")
  if(GIT_DESCRIBE MATCHES "^v?([0-9]+\\.[0-9]+\\.[0-9]+)$")
    # Tagged release version.
    set(GIT_TAG ${CMAKE_MATCH_1})
    if(NOT GIT_TAG VERSION_EQUAL PERCEMON_VERSION)
      message(
        SEND_ERROR
          "percemon version (${PERCEMON_VERSION}) does not match Git tag (${GIT_TAG})."
      )
    endif()
  elseif(GIT_DESCRIBE MATCHES
         "^v?([0-9]+\\.[0-9]+\\.[0-9]+)(-[a-z0-9]+)?-([0-9]+)-g(.+)$")
    # Untagged pre-release. The PERCEMON version is updated to include the
    # number of commits since the last tagged version and the commit hash. The
    # version is formatted in accordance with the https://semver.org
    # specification.
    set(GIT_TAG ${CMAKE_MATCH_1})
    set(GIT_COMMITS_AFTER_TAG ${CMAKE_MATCH_3})
    set(GIT_COMMIT ${CMAKE_MATCH_4})
    if(NOT PERCEMON_VERSION VERSION_GREATER GIT_TAG)
      message(
        SEND_ERROR
          "percemon version (${PERCEMON_VERSION}) must be greater than tagged ancestor (${GIT_TAG})."
      )
    endif()
    set(PERCEMON_FULL_VERSION
        "${PERCEMON_VERSION}-dev${GIT_COMMITS_AFTER_TAG}+${GIT_COMMIT}"
        CACHE STRING "percemon library version (with pre-release metadata)"
              FORCE)
  else()
    message(SEND_ERROR "Failed to parse version from output of `git describe`.")
  endif()
else()
  message(AUTHOR_WARNING "Failed to match tag using git. Using hard-coded")
endif()
