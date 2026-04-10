cmake_minimum_required(VERSION 3.15...4.0)

project(myca)

option(USE_SANITIZERS "Whether to enable UBSAN and ASAN" OFF)
set(CMAKE_CXX_STANDARD 23)
add_compile_options(-Wall -Wextra -Wpedantic)

if(USE_SANITIZERS)
	add_link_options(-fsanitize=address,undefined)
	add_compile_options(-fsanitize=address,undefined)
endif()

# Common object files shared by `myca` (the compiler) and `unit_tests`
add_library(myca-common OBJECT
	src/codegen/c_backend.cpp
	src/syntax/ast.cpp
	src/syntax/lexer.cpp
	src/syntax/parser.cpp
	src/semantics/ast_operations.cpp
	src/semantics/constraint_solver.cpp
	src/semantics/context.cpp
	src/semantics/instance_registry.cpp
	src/semantics/module.cpp
	src/semantics/pass__default_value_checking.cpp
	src/semantics/pass__layout_computation.cpp
	src/semantics/pass__name_registration.cpp
	src/semantics/pass__name_resolution.cpp
	src/semantics/pass__typechecking.cpp
	src/semantics/passes.cpp
	src/semantics/substitution.cpp
	src/semantics/type_env.cpp
	src/semantics/type_properties.cpp
	src/semantics/unification.cpp
	src/semantics/well_formed_term_set.cpp
)
target_include_directories(myca-common PUBLIC src)
target_precompile_headers(myca-common PUBLIC src/utils.hpp)

add_executable(myca src/main.cpp $<TARGET_OBJECTS:myca-common>)
target_include_directories(myca PUBLIC src)

add_executable(unit_tests
	tests/unit/runner.cpp
	tests/unit/test_constraints.cpp
	tests/unit/test_comments.cpp
	$<TARGET_OBJECTS:myca-common>
)
target_include_directories(unit_tests PUBLIC src)

add_executable(integration_tests tests/integration/runner.cpp $<TARGET_OBJECTS:myca-common>)
target_include_directories(integration_tests PUBLIC src)

# Custom targets for running tests
add_custom_target(run-unit-tests $<TARGET_FILE:unit_tests>)
add_custom_target(run-integration-tests
	$<TARGET_FILE:integration_tests> ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR}
)

add_custom_target(run-tests)
add_dependencies(run-tests run-unit-tests)
add_dependencies(run-tests run-integration-tests)
