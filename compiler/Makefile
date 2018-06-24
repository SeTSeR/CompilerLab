CFLAGS = -g -Iinclude -c -std=gnu99
LDFLAGS = -ldl -lm -lpthread
TSTLDFLAGS = -lm
SRC_DIR = src
BUILD_DIR = ../build/compiler
TESTS_DIR = tests
TESTS_BUILD_DIR = $(BUILD_DIR)/tests
SRC = $(wildcard $(SRC_DIR)/*.c)
OBJ = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SRC))
TST = $(wildcard $(TESTS_DIR)/*.c)
TST_OBJ = $(patsubst $(TESTS_DIR)/%.c,$(TESTS_BUILD_DIR)/%.o,$(TST))
TESTED_OBJ = $(filter-out $(BUILD_DIR)/main.o, $(OBJ))
EXE = compiler
TST_EXE = tester

all: dir $(EXE)

tests: testdir $(TST_EXE)
	./$(TST_EXE) 2>>$(TESTS_BUILD_DIR)/err_log.txt
	rm $(TST_EXE)

$(EXE): $(OBJ)
	$(CC) $^ -o $@ $(LDFLAGS)

$(TST_EXE): $(TST_OBJ) $(TESTED_OBJ)
	$(CC) $^ -o $@ $(TSTLDFLAGS)

dir:
	mkdir -p $(BUILD_DIR)

testdir:
	mkdir -p $(TESTS_BUILD_DIR)

$(OBJ): $(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@

$(TST_OBJ): $(TESTS_BUILD_DIR)/%.o: $(TESTS_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@
