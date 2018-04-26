CFLAGS = -g -Iinclude -c
LDFLAGS = -ldl -lm
SRC_DIR = src
BUILD_DIR = ../build
SRC = $(wildcard $(SRC_DIR)/*.c)
OBJ = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SRC))
EXE = compiler

ifeq ($(SOLVE_METHOD), binary)
	CFLAGS += -DSOLVE_BINARY
endif

all: dir $(EXE)

$(EXE): $(OBJ)
	$(CC) $^ -o $@ $(LDFLAGS)

dir:
	mkdir -p $(BUILD_DIR)

$(OBJ): $(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@
