#include "optimizer.h"

#include <math.h>
#include <stdbool.h>
#include <stdlib.h>

static void fold_constants(AST* tree) {
	if(tree == NULL) return;
	if(is_operator(tree)) {
		fold_constants(tree->first_param);
		fold_constants(tree->second_param);
		switch(tree->op_type) {
			case PLUS:
				if(is_number(tree->first_param) && is_number(tree->second_param)) {
					tree->type = NUMBER;
					double value = tree->first_param->value + tree->second_param->value;
					destroy_tree(tree->first_param);
					destroy_tree(tree->second_param);
					tree->value = value;
				}
				break;
			case MINUS:
				if(is_number(tree->first_param) && is_number(tree->second_param)) {
					tree->type = NUMBER;
					double value = tree->first_param->value - tree->second_param->value;
					destroy_tree(tree->first_param);
					destroy_tree(tree->second_param);
					tree->value = value;
				}
				break;
			case MULTIPLY:
				if(is_number(tree->first_param) && is_number(tree->second_param)) {
					tree->type = NUMBER;
					double value = tree->first_param->value * tree->second_param->value;
					destroy_tree(tree->first_param);
					destroy_tree(tree->second_param);
					tree->value = value;
				}
				break;
			case DIVIDE:
				if(is_number(tree->first_param) && is_number(tree->second_param)) {
					tree->type = NUMBER;
					double value = tree->first_param->value / tree->second_param->value;
					destroy_tree(tree->first_param);
					destroy_tree(tree->second_param);
					tree->value = value;
				}
				break;
			case SIN:
				if(is_number(tree->first_param)) {
					tree->type = NUMBER;
					double value = sin(tree->first_param->value);
					destroy_tree(tree->first_param);
					tree->value = value;
				}
				break;
			case COS:
				if(is_number(tree->first_param)) {
					tree->type = NUMBER;
					double value = cos(tree->first_param->value);
					destroy_tree(tree->first_param);
					tree->value = value;
				}
				break;
			case TAN:
				if(is_number(tree->first_param)) {
					tree->type = NUMBER;
					double value = tan(tree->first_param->value);
					destroy_tree(tree->first_param);
					tree->value = value;
				}
				break;
			case CTG:
				if(is_number(tree->first_param)) {
					tree->type = NUMBER;
					double value = 1/tan(tree->first_param->value);
					destroy_tree(tree->first_param);
					tree->value = value;
				}
				break;
			default:
				break;
		}
	}
}

static void optimize_arithmetic(AST* tree) {
	if(tree == NULL) return;
	if(tree->type == OPERATOR) {
		optimize_arithmetic(tree->first_param);
		optimize_arithmetic(tree->second_param);
		switch(tree->op_type) {
			case PLUS:
				if(is_zero(tree->first_param)) {
					free(tree->first_param);
					move_ast(tree, tree->second_param);
				}
				else if(is_zero(tree->second_param)) {
					free(tree->second_param);
					move_ast(tree, tree->first_param);
				}
				break;
			case MINUS:
				if(is_zero(tree->second_param)) {
					free(tree->second_param);
					move_ast(tree, tree->first_param);
				}
				if(equal(tree->first_param, tree->second_param)) {
					free(tree->first_param);
					free(tree->second_param);
					tree->type = NUMBER;
					tree->value - 0;
				}
				break;
			case MULTIPLY:
				if(is_number(tree->first_param)) {
					if(is_zero(tree->first_param)) {
						free(tree->first_param);
						destroy_tree(tree->second_param);
						tree->type = NUMBER;
						tree->value = 0;
					}
					else if(is_one(tree->first_param)) {
						free(tree->first_param);
						move_ast(tree, tree->second_param);
					}
				}
				if(is_number(tree->second_param)) {
					if(is_zero(tree->second_param)) {
						free(tree->second_param);
						destroy_tree(tree->first_param);
						tree->type = NUMBER;
						tree->value = 0;
					}
					else if(is_one(tree->second_param)) {
						free(tree->second_param);
						move_ast(tree, tree->first_param);
					}
				}
				break;
			case DIVIDE:
				if(is_zero(tree->first_param)) {
					free(tree->first_param);
					destroy_tree(tree->second_param);
					tree->type = NUMBER;
					tree->value = 0;
				}
				if(is_one(tree->second_param)) {
					free(tree->second_param);
					move_ast(tree, tree->first_param);
				}
				if(equal(tree->first_param, tree->second_param)) {
					free(tree->first_param);
					free(tree->second_param);
					tree->type = NUMBER;
					tree->value = 1;
				}
				break;
			default:
				break;
		}
	}
}

void optimize(AST* tree) {
	fold_constants(tree);
	optimize_arithmetic(tree);
}
