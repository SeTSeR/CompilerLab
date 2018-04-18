CFLAGS = -g -I../include
LDFLAGS = -lm
OBJ = analytic.o functions.o main.o

all: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o solver

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
