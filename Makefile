SPEC_FILE ?= input.txt
YASMFLAGS = -g dwarf2 -DUNIX -felf64
METHOD ?= newton
BUILD_DIR = build
OUTPUT_DIR = out
GENERATED_ASM = output.asm
COMPILER ?= compiler

all: build

build: dir solver

run: build
	$(OUTPUT_DIR)/main -f $(OUTPUT_DIR)/libfunctions.so

test: build compiler_tests compiler-haskell_tests compiler-rust_tests solver_tests

compiler_tests:
	make tests -C compiler

compiler-haskell_tests:
	cd compiler-haskell; stack test

compiler-rust_tests:
	cd compiler-rust; cargo test

solver_tests:
	make tests -C solver

dir:
	mkdir -p $(OUTPUT_DIR)
	mkdir -p $(BUILD_DIR)

clean:
	-cd compiler-haskell ; stack clean
	-cd compiler-rust ; cargo clean
	rm -r $(OUTPUT_DIR)
	rm -r $(BUILD_DIR)

libfunctions.so: $(COMPILER) $(SPEC_FILE)
	$(OUTPUT_DIR)/compiler $(SPEC_FILE) $(OUTPUT_DIR)/$(GENERATED_ASM)
	yasm $(YASMFLAGS) $(OUTPUT_DIR)/$(GENERATED_ASM) -o $(BUILD_DIR)/functions.o
	gcc -shared -o $(OUTPUT_DIR)/libfunctions.so $(BUILD_DIR)/functions.o

solver: libfunctions.so
	make -C solver
	mv solver/solver $(OUTPUT_DIR)/main

compiler:
	make -C compiler
	mv compiler/compiler $(OUTPUT_DIR)/compiler

compiler-haskell:
	cd compiler-haskell; stack build; stack install
	mv $(HOME)/.local/bin/CompilerLab-exe $(OUTPUT_DIR)/compiler

compiler-rust:
	cd compiler-rust; cargo build
	cp compiler-rust/target/debug/compiler-rust $(OUTPUT_DIR)/compiler

.PHONY: solver compiler tests compiler-haskell compiler-rust all clean
