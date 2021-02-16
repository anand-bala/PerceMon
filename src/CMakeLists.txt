# AST sources
add_percemon_component(percemon_ast ast/ast.cc)
target_include_directories(percemon_ast PRIVATE ${CMAKE_CURRENT_LIST_DIR}/ast)
target_link_libraries(percemon_ast PUBLIC fmt::fmt)

# Parser sources
add_percemon_component(
  percemon_parser
  parser/actions.hpp
  parser/error_messages.hpp
  parser/parser.cc
  parser/grammar.cc
  parser/grammar.hpp
  parser/parse_tree.cc
  parser/parse_tree.hpp
)
target_include_directories(percemon_parser PRIVATE ${CMAKE_CURRENT_LIST_DIR}/parser)
target_link_libraries(percemon_parser PRIVATE taocpp::pegtl range-v3::range-v3)
target_link_libraries(percemon_parser PUBLIC fmt::fmt)
set_std_filesystem_options(percemon_parser)

add_percemon_component(
  percemon_algorithms algorithms/default_monitor.cc algorithms/horizon.cc
  algorithms/topo.cc
)
target_include_directories(
  percemon_algorithms PRIVATE ${CMAKE_CURRENT_LIST_DIR}/algorithms
)
target_link_libraries(percemon_algorithms PRIVATE range-v3::range-v3)

add_library(PerceMon libpercemon.cc)
add_library(PerceMon::PerceMon ALIAS PerceMon)
target_link_libraries(PerceMon PUBLIC percemon_ast percemon_parser percemon_algorithms)

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
