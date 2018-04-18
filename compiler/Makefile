CFLAGS = -g -I../include
OBJ = parser.o optimizer.o codegen.o main.o

all: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o compiler

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
