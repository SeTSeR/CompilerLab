SPEC_FILE ?= input.txt
YASMFLAGS = -g dwarf2 -DUNIX -felf64
METHOD ?= newton

all: solver
	mv solver/solver main

clean:
	rm output.asm
	rm main
	rm libfunctions.so
	rm compiler/compiler
	rm solver/*.o
	rm compiler/*.o

libfunctions.so: compiler $(SPEC_FILE)
	compiler/compiler $(SPEC_FILE) output.asm
	yasm $(YASMFLAGS) output.asm -o solver/functions.o
	gcc -shared -o libfunctions.so solver/functions.o

solver: libfunctions.so
	make -C solver

compiler:
	make -C compiler

.PHONY: solver compiler all clean
