SPEC_FILE ?= input.txt
YASMFLAGS = -g dwarf2 -DUNIX -felf64
METHOD ?= newton
BUILD_DIR = build
OUTPUT_DIR = out
GENERATED_ASM = output.asm

all: build

build: dir solver

run: build
	$(OUTPUT_DIR)/main -f $(OUTPUT_DIR)/libfunctions.so

tests: build compiler_tests solver_tests

compiler_tests:
	make tests -C compiler

solver_tests:
	make tests -C solver

dir:
	mkdir -p $(OUTPUT_DIR)

clean:
	rm -r $(OUTPUT_DIR)
	rm -r $(BUILD_DIR)

libfunctions.so: compiler $(SPEC_FILE)
	$(OUTPUT_DIR)/compiler $(SPEC_FILE) $(OUTPUT_DIR)/$(GENERATED_ASM)
	yasm $(YASMFLAGS) $(OUTPUT_DIR)/$(GENERATED_ASM) -o $(BUILD_DIR)/functions.o
	gcc -shared -o $(OUTPUT_DIR)/libfunctions.so $(BUILD_DIR)/functions.o

solver: libfunctions.so
	make -C solver
	mv solver/solver $(OUTPUT_DIR)/main

compiler:
	make -C compiler
	mv compiler/compiler $(OUTPUT_DIR)/compiler

.PHONY: solver compiler all clean
