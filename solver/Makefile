CFLAGS = -g -I../include
LDFLAGS = -ldl
OBJ = analytic.o main.o functions.o

ifeq ($(METHOD), binary)
	CFLAGS += -DSOLVE_BINARY
endif

all: $(OBJ)
	$(CC) $(OBJ) -o solver $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
