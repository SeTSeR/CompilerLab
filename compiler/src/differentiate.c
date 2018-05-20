#include "differentiate.h"

#include <stdio.h>
#include <stdlib.h>

AST* derivative(AST* function) {
	AST* ans = create_tree();
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
					AST* laddend = create_tree();
					AST* raddend = create_tree();
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
					AST* nominator = create_tree();
					AST* denominator = create_tree();
					nominator->type = OPERATOR;
					nominator->op_type = MINUS;
					laddend = create_tree();
					raddend = create_tree();
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
					AST* leftarg = create_tree();
					AST* rightarg = derivative(function->first_param);
					leftarg->type = OPERATOR;
					leftarg->op_type = COS;
					leftarg->first_param = copy_ast(function->first_param);
					ans->first_param = leftarg;
					ans->second_param = rightarg;
					break;
				case COS:
					ans->op_type = MINUS;
					leftarg = create_tree();
					rightarg = create_tree();
					leftarg->type = NUMBER;
					leftarg->value = 0;
					rightarg->type = OPERATOR;
					rightarg->op_type = MULTIPLY;
					AST* rlarg = create_tree();
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
					rightarg = create_tree();
					rlarg = create_tree();
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
					leftarg = create_tree();
					rightarg = create_tree();
					leftarg->type = NUMBER;
					leftarg->value = 0;
					rightarg->type = OPERATOR;
					rightarg->op_type = DIVIDE;
					rrarg = create_tree();
					rlarg = create_tree(); //In fact rrlarg, but there are too many variables.
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
					fprintf(stderr, "Unknown operator type: %d\n", function->op_type);
					exit(EXIT_FAILURE);
					break;
			}
			break;
		default:
			fprintf(stderr, "Unknown node type: %d\n", function->type);
			exit(EXIT_FAILURE);
			break;
	}
	return ans;
}
