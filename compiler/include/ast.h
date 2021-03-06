#ifndef _AST_H
#define _AST_H

#include <stdbool.h>
#include <stdio.h>

enum TOKEN_TYPE {
	VARIABLE,
	NUMBER,
	OPERATOR
};

enum OPERATOR_TYPE {
	PLUS,
	MINUS,
	MULTIPLY,
	DIVIDE,
	POWER,
	SIN,
	COS,
	TAN,
	CTG,
	LN
};

struct AST {
	int type;
	union {
		double value;
		struct {
			int op_type;
			struct AST *first_param;
			struct AST *second_param;
		};
	};
};

typedef struct AST AST;

AST *copy_ast(AST*);
AST *create_tree();
void move_ast(AST*, AST*);
void destroy_tree(AST*);
void print_tree(AST*, int, FILE*);

bool is_number(AST*);
bool is_operator(AST*);
bool is_variable(AST*);
bool is_zero(AST*);
bool is_one(AST*);


bool equal(AST*, AST*);

#endif
