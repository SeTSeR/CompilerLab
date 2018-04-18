CFLAGS = -g -I../include
OBJ = parser.o optimizer.o codegen.o main.o

all: $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) -o compiler $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
