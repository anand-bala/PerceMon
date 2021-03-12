set(private_deps fmt::fmt magic_enum::magic_enum range-v3::range-v3)

# AST sources
add_percemon_component(percemon_ast ast/expression.cc)
target_include_directories(percemon_ast PRIVATE ${CMAKE_CURRENT_LIST_DIR}/ast)
target_link_libraries(percemon_ast PRIVATE "$<BUILD_INTERFACE:${private_deps}>")

# Parser sources
add_percemon_component(
  percemon_parser
  parser/error_messages.hpp
  parser/grammar.hpp
  parser/parser.cc
  # parser/parse_tree.hpp parser/parse_tree.cc
  parser/actions.hpp
  parser/grammar.cc
  parser/var_lookup.cc
)
target_include_directories(percemon_parser PRIVATE ${CMAKE_CURRENT_LIST_DIR}/parser)
target_link_libraries(
  percemon_parser
  PRIVATE "$<BUILD_INTERFACE:${private_deps};taocpp::pegtl>"
  PUBLIC percemon_ast
)
set_std_filesystem_options(percemon_parser)

add_percemon_component(percemon_topo topo/quadtree.cc topo/topology.cc)
target_link_libraries(percemon_topo PRIVATE "$<BUILD_INTERFACE:${private_deps}>")

add_percemon_component(
  percemon_algorithms algorithms/default_monitor.cc algorithms/horizon.cc
  # algorithms/topo.cc
)
target_include_directories(
  percemon_algorithms PRIVATE ${CMAKE_CURRENT_LIST_DIR}/algorithms
)
target_link_libraries(
  percemon_algorithms
  PUBLIC percemon_ast percemon_topo
  PRIVATE "$<BUILD_INTERFACE:${private_deps}>"
)

add_library(PerceMon libpercemon.cc)
add_library(PerceMon::PerceMon ALIAS PerceMon)
target_link_libraries(
  PerceMon PUBLIC percemon-headers-only percemon_ast percemon_parser percemon_topo
                  percemon_algorithms
)

set_default_compile_options(PerceMon)
set_project_warnings(PerceMon)
enable_clang_tidy(PerceMon)
enable_include_what_you_use(PerceMon)
add_coverage(PerceMon)

install(
  TARGETS PerceMon
  EXPORT PerceMonTargets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
