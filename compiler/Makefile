CFLAGS = -g -Iinclude
OBJ = ast.o symtab.o dynstring.o differentiate.o stackast.o parser.o optimizer.o codegen.o main.o
LDFLAGS = -lm

all: $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) -o compiler $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
