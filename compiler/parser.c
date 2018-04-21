#include "stackast.h"
#include "parser.h"

#include <math.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

AST *parse(char *str) {
	ASTack *stack = calloc(1, sizeof(ASTack*));
	clear(stack);
	char *token = strtok(str, " \n");
	while(token) {
		if(strlen(token) != 0) {
			AST* tokast = calloc(1, sizeof(AST));
			if(isalpha(token[0])) {
				if(token[0] == 'x') {
					tokast->type = VARIABLE;
				}
				else if(strncmp(token, "pi", 2) == 0) {
					tokast->type = NUMBER;
					tokast->value = M_PI;
				}
				else if(strncmp(token, "e", 1) == 0) {
					tokast->type = NUMBER;
					tokast->value = M_E;
				}
				else if(strncmp(token, "sin", 3) == 0) {
					tokast->type = OPERATOR;
					tokast->op_type = SIN;
					AST *argast = pop(stack);
					tokast->first_param = argast;
				}
				else if(strncmp(token, "cos", 3) == 0) {
					tokast->type = OPERATOR;
					tokast->op_type = COS;
					AST *argast = pop(stack);
					tokast->first_param = argast;
				}	
				else if(strncmp(token, "tan", 3) == 0) {
					tokast->type = OPERATOR;
					tokast->op_type = TAN;
					AST *argast = pop(stack);
					tokast->first_param = argast;
				}
				else if(strncmp(token, "ctg", 3) == 0) {
					tokast->type = OPERATOR;
					tokast->op_type = CTG;
					AST *argast = pop(stack);
					tokast->first_param = argast;
				}		
			}
			else if(isdigit(token[0])) {
				tokast->type = NUMBER;
				sscanf(token, "%lf", &(tokast->value));
			}
			else {
				AST *firstarg, *secondarg;
				secondarg = pop(stack);
				firstarg = pop(stack);
				tokast->type = OPERATOR;
				tokast->first_param = firstarg;
				tokast->second_param = secondarg;
				switch(token[0]) {
					case '+':
						tokast->op_type = PLUS;
						break;
					case '-':
						tokast->op_type = MINUS;
						break;
					case '*':
						tokast->op_type = MULTIPLY;
						break;
					case '/':
						tokast->op_type = DIVIDE;
						break;
					default:
						fprintf(stderr, "Unknown character: %c", token[0]);
						exit(EXIT_FAILURE);
				}
			}
			push(stack, tokast);
		}
		token = strtok(NULL, " \n");
	}
	return pop(stack);
}
