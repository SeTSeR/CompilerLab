#include <stdlib.h>

#include "stackast.h"

enum {
	INITIAL_CAPACITY = 10
};

ASTack *create_stack() {
	ASTack *stack = calloc(1, sizeof(ASTack));
	stack->size = 0;
	stack->capacity = INITIAL_CAPACITY;
	stack->stack = malloc(stack->capacity * sizeof(AST*));
    return stack;
}

void destroy_stack(ASTack *stack) {
	stack->size = 0;
	stack->capacity = 0;
	free(stack->stack);
	free(stack);
}

void clear(ASTack *st) {
	st->size = 0;
	st->capacity = INITIAL_CAPACITY;
	st->stack = realloc(st->stack, st->capacity * sizeof(AST*));
}

void push(ASTack *st, AST *tree) {
	if(st->size == st->capacity) {
		st->capacity <<= 1;
		st->stack = realloc(st->stack, st->capacity * sizeof(AST*));
	}
	st->stack[st->size++] = tree;
}

AST* pop(ASTack *st) {
	if(st->size <= 0) return NULL;
	return st->stack[--st->size];
}

int size(ASTack *st) {
	return st->size;
}
