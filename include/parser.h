#ifndef _PARSER_H
#define _PARSER_H

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
	SIN,
	COS,
	TAN,
	CTG
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

AST *parse(char* s);

#endif
