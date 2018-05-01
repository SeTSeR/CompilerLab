CFLAGS = -g -Iinclude -c
LDFLAGS = -ldl
TSTLDFLAGS = -lm
SRC_DIR = src
BUILD_DIR = ../build/solver
TESTS_DIR = tests
TESTS_BUILD_DIR = $(BUILD_DIR)/tests
SRC = $(wildcard $(SRC_DIR)/*.c)
OBJ = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SRC))
TST = $(wildcard $(TESTS_DIR)/*.c)
TST_OBJ = $(patsubst $(TESTS_DIR)/%.c,$(TESTS_BUILD_DIR)/%.o,$(TST))
EXE = solver
TST_EXE = tester

ifeq ($(SOLVE_METHOD), binary)
	CFLAGS += -DSOLVE_BINARY
endif

all: dir $(EXE)

tests: testdir $(TST_EXE)
	./$(TST_EXE) 2>>$(TESTS_BUILD_DIR)/err_log.txt
	rm $(TST_EXE)

$(EXE): $(OBJ)
	$(CC) $^ -o $@ $(LDFLAGS)

$(TST_EXE): $(TST_OBJ) $(BUILD_DIR)/analytic.o
	$(CC) $^ -o $@ $(TSTLDFLAGS)

dir:
	mkdir -p $(BUILD_DIR)

testdir:
	mkdir -p $(TESTS_BUILD_DIR)

$(OBJ): $(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@

$(TST_OBJ): $(TESTS_BUILD_DIR)/%.o: $(TESTS_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@
