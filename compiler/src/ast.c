#include "ast.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define eps 0.00001

AST* copy_ast(AST* src) {
	if(src == NULL) return NULL;
	AST* ans = calloc(1, sizeof(AST));
	ans->type = src->type;
	switch(src->type) {
		case NUMBER:
			ans->value = src->value;
			break;
		case VARIABLE:
			break;
		case OPERATOR:
			ans->op_type = src->op_type;
			ans->first_param = copy_ast(src->first_param);
			ans->second_param = copy_ast(src->second_param);
			break;
		default:
			fprintf(stderr, "Unknown node type: %d\n", src->type);
			exit(EXIT_FAILURE);
			break;
	}
	return ans;
}

void move_ast(AST *dest, AST *src) {
	dest->type = src->type;
	switch(src->type) {
		case NUMBER:
			dest->value = src->value;
			break;
		case VARIABLE:
			break;
		case OPERATOR:
			dest->op_type = src->op_type;
			dest->first_param = src->first_param;
			dest->second_param = src->second_param;
			break;
		default:
			fprintf(stderr, "Unknown node type: %d\n", src->type);
			exit(EXIT_FAILURE);
			break;
	}
	free(src);
}

bool equal(AST *first, AST *second) {
	if(first == NULL) {
		return (second == NULL);
	}
	if(second == NULL) return false;
	if(first->type != second->type) return false;
	switch(first->type) {
		case NUMBER:
			return (fabs(first->value - second->value < eps));
			break;
		case VARIABLE:
            return true;
			break;
		case OPERATOR:
			if(first->op_type != second->op_type) return false;
			return (equal(first->first_param, second->first_param) && (equal(first->second_param, second->second_param)));
			break;
		default:
            fprintf(stderr, "Unknown tree type: %d\n", first->type);
            exit(EXIT_FAILURE);
			break;
	}
	return true;
}

AST *create_tree() {
	return calloc(1, sizeof(AST));
}

void destroy_tree(AST *tree) {
	if(tree == NULL) return;
	switch(tree->type) {
		case NUMBER:
		case VARIABLE:
			break;
		case OPERATOR:
			destroy_tree(tree->first_param);
			destroy_tree(tree->second_param);
			break;
		default:
			fprintf(stderr, "Unknown node type: %d\n", tree->type);
			exit(EXIT_FAILURE);
			break;
	}
	free(tree);
}

void print_tree(AST* tree, int level, FILE* log) {
	char spaces[128] = "";
	memset(spaces, ' ', level);
	if(tree) {
		switch(tree->type) {
			case VARIABLE:
				fprintf(log, "%sNode type: variable\n", spaces);
				fprintf(log, "%sNode value: x\n", spaces);
				break;
			case NUMBER:
				fprintf(log, "%sNode type: number\n", spaces);
				fprintf(log, "%sNode value: %lf\n", spaces, tree->value);
				break;
			case OPERATOR:
				fprintf(log, "%sNode type: operator\n", spaces);
				char* operator_type;
				switch(tree->op_type) {
					case PLUS:
						operator_type = "+";
						break;
					case MINUS:
						operator_type = "-";
						break;
					case MULTIPLY:
						operator_type = "*";
						break;
					case DIVIDE:
						operator_type = "/";
						break;
					case SIN:
						operator_type = "sin";
						break;
					case COS:
						operator_type = "cos";
						break;
					case TAN:
						operator_type = "tan";
						break;
					case CTG:
						operator_type = "ctg";
						break;
				}
				fprintf(log, "%sNode value: %s\n", spaces, operator_type);
				print_tree(tree->first_param, level + 1, log);
				print_tree(tree->second_param, level + 1, log);
				break;
			default:
				fprintf(stderr, "Unknown node type: %d\n", tree->type);
				exit(EXIT_FAILURE);
				break;
		}
	}
}

bool is_number(AST* tree) {
	return tree->type == NUMBER;
}

bool is_operator(AST* tree) {
	return tree->type == OPERATOR;
}

bool is_variable(AST* tree) {
	return tree->type == VARIABLE;
}

bool is_zero(AST* tree) {
	return is_number(tree) && (fabs(tree->value) < eps);
}

bool is_one(AST* tree) {
	return is_number(tree) && (fabs(tree->value - 1) < eps);
}
