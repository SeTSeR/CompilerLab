#include "differentiate.h"

#include <stdio.h>
#include <stdlib.h>

AST* derivative(AST* function) {
	AST* ans = calloc(1, sizeof(AST));
	switch(function->type) {
		case NUMBER:
			ans->type = NUMBER;
			ans->value = 0;
			break;
		case VARIABLE:
			ans->type = NUMBER;
			ans->value = 1;
			break;
		case OPERATOR:
			ans->type = OPERATOR;
			switch(function->op_type) {
				case PLUS:
				case MINUS:
					ans->op_type = function->op_type;
					ans->first_param = derivative(function->first_param);
					ans->second_param = derivative(function->second_param);
					break;
				case MULTIPLY:
					ans->op_type = PLUS;
					AST* laddend = calloc(1, sizeof(AST));
					AST* raddend = calloc(1, sizeof(AST));
					laddend->type = OPERATOR;
					laddend->op_type = MULTIPLY;
					laddend->first_param = derivative(function->first_param);
					laddend->second_param = copy_ast(function->second_param);
					raddend->type = OPERATOR;
					raddend->op_type = MULTIPLY;
					raddend->first_param = copy_ast(function->first_param);
					raddend->second_param = derivative(function->second_param);
					ans->first_param = laddend;
					ans->second_param = raddend;
					break;
				case DIVIDE:
					ans->op_type = DIVIDE;
					AST* nominator = calloc(1, sizeof(AST));
					AST* denominator = calloc(1, sizeof(AST));
					nominator->type = OPERATOR;
					nominator->op_type = MINUS;
					laddend = calloc(1, sizeof(AST));
					raddend = calloc(1, sizeof(AST));
					laddend->type = OPERATOR;
					laddend->op_type = MULTIPLY;
					laddend->first_param = derivative(function->first_param);
					laddend->second_param = copy_ast(function->second_param);
					raddend->type = OPERATOR;
					raddend->op_type = MULTIPLY;
					raddend->first_param = copy_ast(function->first_param);
					raddend->second_param = derivative(function->second_param);
					nominator->first_param = laddend;
					nominator->second_param = raddend;
					denominator->type = OPERATOR;
					denominator->op_type = MULTIPLY;
					denominator->first_param = copy_ast(function->second_param);
					denominator->second_param = copy_ast(function->second_param);
					ans->first_param = nominator;
					ans->second_param = denominator;
					break;
				case SIN:
					ans->op_type = MULTIPLY;
					AST* leftarg = calloc(1, sizeof(AST));
					AST* rightarg = derivative(function->first_param);
					leftarg->type = OPERATOR;
					leftarg->op_type = COS;
					leftarg->first_param = copy_ast(function->first_param);
					ans->first_param = leftarg;
					ans->second_param = rightarg;
					break;
				case COS:
					ans->op_type = MINUS;
					leftarg = calloc(1, sizeof(AST));
					rightarg = calloc(1, sizeof(AST));
					leftarg->type = NUMBER;
					leftarg->value = 0;
					rightarg->type = OPERATOR;
					rightarg->op_type = MULTIPLY;
					AST* rlarg = calloc(1, sizeof(AST));
					AST* rrarg = derivative(function->first_param);
					rlarg->type = OPERATOR;
					rlarg->op_type = SIN;
					rlarg->first_param = copy_ast(function->first_param);
					rightarg->first_param = rlarg;
					rightarg->second_param = rrarg;
					ans->first_param = leftarg;
					ans->second_param = rightarg;
					break;
				case TAN:
					ans->op_type = DIVIDE;
					rightarg = calloc(1, sizeof(AST));
					rlarg = calloc(1, sizeof(AST));
					rlarg->type = OPERATOR;
					rlarg->op_type = COS;
					rlarg->first_param = copy_ast(function->first_param);
					rightarg->type = OPERATOR;
					rightarg->op_type = MULTIPLY;
					rightarg->first_param = rlarg;
					rightarg->second_param = copy_ast(rlarg);
					ans->first_param = derivative(function->first_param);
					ans->second_param = rightarg;
					break;
				case CTG:
					ans->op_type = MINUS;
					leftarg = calloc(1, sizeof(AST));
					rightarg = calloc(1, sizeof(AST));
					leftarg->type = NUMBER;
					leftarg->value = 0;
					rightarg->type = OPERATOR;
					rightarg->op_type = DIVIDE;
					rrarg = calloc(1, sizeof(AST));
					rlarg = calloc(1, sizeof(AST)); //In fact rrlarg, but there are too many variables.
					rlarg->type = OPERATOR;
					rlarg->op_type = SIN;
					rlarg->first_param = copy_ast(function->first_param);
					rrarg->type = OPERATOR;
					rrarg->op_type = MULTIPLY;
					rrarg->first_param = rlarg;
					rrarg->second_param = copy_ast(rlarg);
					rightarg->first_param = derivative(function->first_param);
					rightarg->second_param = rrarg;
					ans->first_param = leftarg;
					ans->second_param = rightarg;
					break;
				default:
					fprintf(stderr, "Unknown operator type: %d", function->op_type);
					exit(EXIT_FAILURE);
					break;
			}
			break;
		default:
			fprintf(stderr, "Unknown node type: %d", function->type);
			exit(EXIT_FAILURE);
			break;
	}
	return ans;
}
