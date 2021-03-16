message(STATUS "Building Tests in ${CMAKE_CURRENT_LIST_DIR}")
# include(Catch)

add_executable(
  percemon_tests parser/test_grammar.cc parser/test_parsing.cc test_main.cc
                 utils/test_visit.cc # ast/test_in_source_expressions.cc
)
target_include_directories(
  percemon_tests PRIVATE ${CMAKE_CURRENT_LIST_DIR} ${PROJECT_SOURCE_DIR}/src
)
target_link_libraries(percemon_tests PRIVATE percemon Catch2::Catch2 taocpp::pegtl)
if(MSVC)
  target_compile_options(percemon_tests PRIVATE /W4 /WX /utf-8)
else()
  target_compile_options(percemon_tests PRIVATE -pedantic -Wall -Wextra -Werror)
endif()

add_coverage(percemon_tests)
catch_discover_tests(percemon_tests)
