c_temp = /tmp/_asm_build_$$.c
o_temp = /tmp/_asm_build_$$.o
YASMFLAGS = -g dwarf2 -DUNIX -felf64
CFLAGS = -g -Iinclude/
LDFLAGS = -lm
OBJ = main.o analytic.o

all: solver
	mv solver/solver .

clean:
	rm output.asm
	rm solver
	rm solver/*.o
	rm compiler/*.o

solver: compiler input.txt
	compiler/compiler input.txt output.asm
	yasm $(YASMFLAGS) output.asm -o solver/functions.o
	make -C solver

compiler:
