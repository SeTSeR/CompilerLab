SPEC_FILE ?= input.txt
YASMFLAGS = -g dwarf2 -DUNIX -felf64

all: solver
	mv solver/solver main

clean:
	rm output.asm
	rm main
	rm compiler/compiler
	rm solver/*.o
	rm compiler/*.o

solver: compiler $(SPEC_FILE)
	compiler/compiler $(SPEC_FILE) output.asm
	yasm $(YASMFLAGS) output.asm -o solver/functions.o
	make -C solver

compiler:
	make -C compiler
