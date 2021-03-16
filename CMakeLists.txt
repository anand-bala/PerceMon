cmake_minimum_required(VERSION 3.11)

# Check if percemon is being used directly or via add_subdirectory
set(PERCEMON_MASTER_PROJECT OFF)
if(CMAKE_CURRENT_LIST_DIR STREQUAL CMAKE_SOURCE_DIR)
  set(PERCEMON_MASTER_PROJECT ON)
endif()

include(cmake/Version.cmake)
message(STATUS "Configuring percemon: v${PERCEMON_FULL_VERSION}")

project(
  percemon
  VERSION ${PERCEMON_VERSION}
  DESCRIPTION
    "A library for efficient online monitoring for spatio-temporal quality logic on streams of perception data."
  LANGUAGES CXX
)

include(CMakeDependentOption)

# Options
option(ENABLE_TESTING "Build percemon test suite?" ${PERCEMON_MASTER_PROJECT})
option(BUILD_EXAMPLES "Build the examples?" ${PERCEMON_MASTER_PROJECT})
# Coverage needs to be explicitly turned on, but available only if Testing is enabled.
cmake_dependent_option(
  ENABLE_COVERAGE "Generate coverage information from test suite?" ON "ENABLE_TESTING"
  OFF
)
option(ENABLE_CACHE "Enable cache if available" ${PERCEMON_MASTER_PROJECT})

# ######################################################################################
# Include CMake Modules  #
# ######################################################################################

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(StandardProjectSettings)
include(PreventInSourceBuilds)
include(CompilerWarnings)
include(Cache)
include(StaticAnalyzers)

include(CTest)
include(Codecov)

include(GNUInstallDirs)

# ######################################################################################
# Some Global Configuration  #
# ######################################################################################

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)

# ######################################################################################
# Third Party dependencies  #
# ######################################################################################

include(ThirdPartyDeps)

# ######################################################################################
# Helper targets/functions  #
# ######################################################################################

set(percemon_headers
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/algorithms.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/algorithms/default_monitor.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/ast_fwd.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/details/attributes.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/details/functions.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/details/primitives.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/details/propositional.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/details/spatial.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/details/temporal.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/ast/expression.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/datastream.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/exception.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/internal/filesystem.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/parser.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/percemon.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo/details/base.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo/details/bbox.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo/details/primitives.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo/details/quadtree.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo/details/topology.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo/details/vector2.hpp
    ${CMAKE_CURRENT_LIST_DIR}/include/percemon/topo/topo_fwd.hpp
)

add_library(percemon-headers-only INTERFACE)
target_sources(percemon-headers-only INTERFACE "$<BUILD_INTERFACE:${percemon_headers}>")
target_include_directories(
  percemon-headers-only
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
)
target_include_directories(
  percemon-headers-only SYSTEM
  INTERFACE $<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_INCLUDEDIR}>
)

install(
  TARGETS percemon-headers-only
  EXPORT percemonTargets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# Add an OBJECT library for a component of libpercemon. A thin wrapper around
# `add_library(OBJECT)`. Thus, it takes as the first argument a `target` name, and the
# rest is assumed to be the sources making up the Object Library.
function(add_percemon_component target)
  add_library(${target} OBJECT ${ARGN})
  target_link_libraries(${target} PUBLIC percemon-headers-only)
  target_include_directories(
    ${target} PRIVATE $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
  )
  target_compile_features(${target} PUBLIC cxx_std_17)
  set_default_compile_options(${target})
  set_project_warnings(${target})
  add_coverage(${target})
endfunction()

# Link to dependencies privately, i.e., don't install it.
function(privately_link target)
  target_link_libraries(${target} PRIVATE "$<BUILD_INTERFACE:${ARGN}>")
endfunction()

# ######################################################################################
# Sources  #
# ######################################################################################

add_subdirectory(src)

set(_PERCEMON_BUILD_THE_TESTS
    OFF
    CACHE INTERNAL "Easy option to build the tests"
)
if((PERCEMON_MASTER_PROJECT AND ENABLE_TESTING) AND BUILD_TESTING)
  set(_PERCEMON_BUILD_THE_TESTS
      ON
      CACHE INTERNAL "Easy option to build the tests"
  )
endif()

if(_PERCEMON_BUILD_THE_TESTS)
  add_subdirectory(tests)
endif()

if(PERCEMON_EXAMPLES)
  add_subdirectory(examples)
endif()

# ######################################################################################
# Installation  #
# ######################################################################################

include(CMakePackageConfigHelpers)

set(PERCEMON_CMAKECONFIG_INSTALL_DIR
    "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
    CACHE STRING "install path for percemonConfig.cmake"
)
set(INSTALL_CONFIGDIR ${PERCEMON_CMAKECONFIG_INSTALL_DIR})

# Install catkin package.xml
install(FILES package.xml DESTINATION ${CMAKE_INSTALL_DATAROOTDIR})

install(
  EXPORT percemonTargets
  FILE percemonTargets.cmake
  NAMESPACE percemon::
  DESTINATION ${INSTALL_CONFIGDIR}
)

# Create a ConfigVersion.cmake file
write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/percemonConfigVersion.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion
)

configure_package_config_file(
  ${CMAKE_CURRENT_LIST_DIR}/cmake/percemonConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/percemonConfig.cmake
  INSTALL_DESTINATION ${INSTALL_CONFIGDIR}
)

# Install the include dir
install(DIRECTORY include/percemon TYPE INCLUDE)

# Install the config, configversion and custom find modules
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/percemonConfig.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/percemonConfigVersion.cmake
        DESTINATION ${INSTALL_CONFIGDIR}
)

install(EXPORT percemonTargets DESTINATION ${INSTALL_CONFIGDIR})

install(FILES LICENSE LICENSE-3rdParty DESTINATION ${CMAKE_INSTALL_DOCDIR})
