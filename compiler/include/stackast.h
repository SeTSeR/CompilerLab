#include "ast.h"

typedef struct ASTack {
	AST **stack;
	int size;
	int capacity;
} ASTack;

void clear(ASTack*);
void push(ASTack*, AST*);
AST* pop(ASTack*);
int size(ASTack*);

ASTack* create_stack();
void destroy_stack(ASTack*);
