SPEC_FILE ?= input.txt
YASMFLAGS = -g dwarf2 -DUNIX -felf64
METHOD ?= newton
BUILD_DIR = build

all: solver
	mv solver/solver main

clean:
	rm output.asm
	rm main
	rm libfunctions.so
	rm compiler/compiler
	rm -r $(BUILD_DIR)

libfunctions.so: compiler $(SPEC_FILE)
	compiler/compiler $(SPEC_FILE) output.asm
	rm $(BUILD_DIR)/*.o
	yasm $(YASMFLAGS) output.asm -o $(BUILD_DIR)/functions.o
	gcc -shared -o libfunctions.so $(BUILD_DIR)/functions.o

solver: libfunctions.so
	make -C solver

compiler:
	make -C compiler

.PHONY: solver compiler all clean
