BUILD_DIR=build

.PHONY: run RUN_ALWAYS

$(BUILD_DIR)/myca: src/*
	@mkdir -p $(BUILD_DIR)
	g++ -Wall -Wextra -Wpedantic -std=c++20 -g src/main.cpp -o $@

build/examples/raylib: examples/raylib.myca RUN_ALWAYS
	@mkdir -p $(BUILD_DIR)/examples
	./build/myca $< -o $@.c
	gcc -Wall -Wextra -Wpedantic $@.c -o $@ -lraylib

build/examples/%: examples/%.myca RUN_ALWAYS
	@mkdir -p $(BUILD_DIR)/examples
	./build/myca $< -o $@.c
	gcc -Wall -Wextra -Wpedantic $@.c -o $@

run: build/examples/union_types
	./$<
