#include "codegen.h"
#include "differentiate.h"
#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int compile(int argc, char** argv) {
	if(argc < 3) return -1;
	FILE *out = fopen(argv[2], "wt");
	fputs("[BITS 64]\n \
		   default rel\n \
		   global a\n \
		   global b\n \
		   global f1\n \
		   global f2\n \
		   global f3\n \
		   global df1\n \
		   global df2\n \
		   global df3\n \
		   \n \
		   section .rodata\n \
		   a dq -3.0\n \
		   b dq 1.5\n \
		   const1 dq 5.0\n \
		   const2 dq 3.0\n \
		   \n \
		   section .text\n \
		   f1:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld1\n \
		   fld st1\n \
		   fprem\n \
		   f2xm1\n \
		   faddp\n \
		   fscale\n \
		   fstp st1\n \
		   fld1\n \
		   faddp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   f2:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld st0\n \
		   fmulp\n \
		   fld st0\n \
		   fmulp\n \
		   fld qword[rsp - 8]\n \
		   fmulp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   f3:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld1\n \
		   fld qword[rsp - 8]\n \
		   fsubp\n \
		   fld qword[const2]\n \
		   fdivp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   df1:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld1\n \
		   fld st1\n \
		   fprem\n \
		   f2xm1\n \
		   faddp\n \
		   fscale\n \
		   fstp st1\n \
		   fldln2\n \
		   fmulp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   df2:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld st0\n \
		   fmulp\n \
		   fld st0\n \
		   fmulp\n \
		   fld qword[const1]\n \
		   fmulp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   df3:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld1\n \
		   fld qword[const2]\n \
		   fdivp\n \
		   fchs\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n", out);
	fclose(out);
	return 0;
}

static void print_tree(AST* tree, int level, FILE* log) {
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
				break;
		}
	}
}

static void delete_tree(AST* tree) {
	if(tree) {
		switch(tree->type) {
			case NUMBER:
			case VARIABLE:
				free(tree);
				break;
			case OPERATOR:
				delete_tree(tree->first_param);
				if(tree->second_param) delete_tree(tree->second_param);
				free(tree);
				break;
			default:
				break;
		}
	}
}

int main(int argc, char** argv) {
	if(argc < 3) return -1;
	FILE *in = fopen(argv[1], "rt");
	char buf[256];
	fgets(buf, 256, in);
	double a, b;
	sscanf(buf, "%lf %lf", &a, &b);
	AST* trees[6];
	for(int i = 0; i < 3; ++i) {
		fgets(buf, 256, in);
		trees[i] = parse(buf);
		trees[i + 3] = derivative(trees[i]);
	}
	char* names[6] = {"f1", "f2", "f3", "df1", "df2", "df3"};
	char* code = translate(a, b, 6, trees, names);
	FILE *out = fopen(argv[2], "wt");
	fputs(code, out);
	fclose(out);
	return 0;
}
