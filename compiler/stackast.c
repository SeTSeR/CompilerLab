#include <stdlib.h>

#include "stackast.h"

enum {
	INITIAL_CAPACITY = 10
};

void clear(ASTack *st) {
	st->size = 0;
	st->capacity = INITIAL_CAPACITY;
	if(st->stack) st->stack = realloc(st->stack, st->capacity * sizeof(AST*));
	else st->stack = malloc(st->capacity * sizeof(AST*));
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
