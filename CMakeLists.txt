cmake_minimum_required(VERSION 3.15...4.0)

option(USE_SANITIZERS "Whether to enable UBSAN and ASAN" OFF)

project(myca)
	add_compile_options(-Wall -Wextra -Wpedantic -std=c++23)

	if(USE_SANITIZERS)
		add_link_options(-fsanitize=address,undefined)
		add_compile_options(-fsanitize=address,undefined)
	endif()

	add_executable(myca
		src/main.cpp
		src/syntax.cpp
		src/semantics.cpp
		src/codegen.cpp
	)

	add_custom_target(test
		python ${CMAKE_SOURCE_DIR}/tests/runner.py ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR}
	)
