#include "codegen.h"
#include "dynstring.h"
#include "parser.h"
#include "symtab.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define eps 0.000001

enum {
	BUFSIZE = 256,
	LABEL_LEVEL = 0,
	FUNC_LEVEL = 4
};

double predefined[7] = {0.0, 1.0, M_PI, M_LOG2E, M_LN10/M_LN2, M_LN2/M_LN10, M_LN2};
char* commands[7] = {"fldz", "fld1", "fldpi", "fldl2e", "fldl2t", "fldlg2", "fldln2"};

inline static int is_predefined(double value) {
	for(int i = 0; i < 7; ++i) if(fabs(value - predefined[i]) < eps) return i;
	return -1;
}

static void find_identifiers(identifiers_table *table, AST *tree) {
	if(tree) {
		switch(tree->type) {
			case NUMBER:
				if(is_predefined(tree->value) == -1) add_identifier(table, tree->value);
				break;
			case VARIABLE:
				break;
			case OPERATOR:
				find_identifiers(table, tree->first_param);
				find_identifiers(table, tree->second_param);
				break;
			default:
				fprintf(stderr, "Unknown node type: %d", tree->type);
				exit(EXIT_FAILURE);
		}
	}
}

static char* gen_header() {
	string *ans = make_string(BUFSIZE);
	append_line(LABEL_LEVEL, ans, "[BITS 64]");
	append_line(FUNC_LEVEL, ans, "default rel");
	append_line(FUNC_LEVEL, ans, "global a");
	append_line(FUNC_LEVEL, ans, "global b");
	append_line(FUNC_LEVEL, ans, "global f1");
	append_line(FUNC_LEVEL, ans, "global f2");
	append_line(FUNC_LEVEL, ans, "global f3");
	append_line(FUNC_LEVEL, ans, "global df1");
	append_line(FUNC_LEVEL, ans, "global df2");
	append_line(FUNC_LEVEL, ans, "global df3");
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

static char* gen_prolog() {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "movsd qword[rsp - 8], xmm0");
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

static char* gen_epilog() {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "fstp qword[rsp - 8]");
	append_line(FUNC_LEVEL, ans, "movsd xmm0, qword[rsp - 8]");
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

static char* gen_node(AST* node, identifiers_table *table) {
	string *ans = make_string(BUFSIZE);
	char command[128];
	int pred = -1;
	switch(node->type) {
		case NUMBER:
			pred = is_predefined(node->value);
			if((0 <= pred) && (pred < 7)) snprintf(command, 128, "%s", commands[pred]);
			else snprintf(command, 128, "fld qword[%s]", lookup(table, node->value));
			append_line(FUNC_LEVEL, ans, command);
			break;
		case VARIABLE:
			append_line(FUNC_LEVEL, ans, "fld qword[rsp - 8]");
			break;
		case OPERATOR:
			switch(node->op_type) {
				case PLUS:
					append(ans, gen_node(node->first_param, table));
					append(ans, gen_node(node->second_param, table));
					append_line(FUNC_LEVEL, ans, "faddp");
					break;
				case MINUS:
					append(ans, gen_node(node->first_param, table));
					append(ans, gen_node(node->second_param, table));
					append_line(FUNC_LEVEL, ans, "fsubp");
					break;
				case MULTIPLY:
					append(ans, gen_node(node->first_param, table));
					append(ans, gen_node(node->second_param, table));
					append_line(FUNC_LEVEL, ans, "fmulp");
					break;
				case DIVIDE:
					append(ans, gen_node(node->first_param, table));
					append(ans, gen_node(node->second_param, table));
					append_line(FUNC_LEVEL, ans, "fdivp");
					break;
				case SIN:
					append(ans, gen_node(node->first_param, table));
					append_line(FUNC_LEVEL, ans, "fsin");
					break;
				case COS:
					append(ans, gen_node(node->first_param, table));
					append_line(FUNC_LEVEL, ans, "fcos");
					break;
				case TAN:
					append(ans, gen_node(node->first_param, table));
					append_line(FUNC_LEVEL, ans, "fptan");
					append_line(FUNC_LEVEL, ans, "fstp st0");
					break;
				case CTG:
					append(ans, gen_node(node->first_param, table));
					append_line(FUNC_LEVEL, ans, "fptan");
					append_line(FUNC_LEVEL, ans, "fdivp");
					break;
				default:
					fprintf(stderr, "Unknown operator type: %d", node->op_type);
					exit(EXIT_FAILURE);
			}
			break;
		default:
			fprintf(stderr, "Unknown node type: %d", node->type);
			exit(EXIT_FAILURE);
	}
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

static char* gen_function(char* fname, AST* func, identifiers_table *table) {
	string *ans = from_cstring(fname);
	append(ans, ":\n");
	append(ans, gen_prolog());
	append(ans, gen_node(func, table));
	append(ans, gen_epilog());
	append_line(FUNC_LEVEL, ans, "ret");
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

static char* gen_rodata(identifiers_table *table) {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "section .rodata");
	for(int i = 0; i < table->size; ++i) {
		char idline[128];
		snprintf(idline, 128, "%s dq %lf", table->identifiers[i].name, table->identifiers[i].value);
		append_line(FUNC_LEVEL, ans, idline);
	}
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

static char* gen_text(int n, AST **funcs, char **fnames, identifiers_table *table) {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "section .text");
	for(int i = 0; i < n; ++i) {
		append(ans, gen_function(fnames[i], funcs[i], table));
		append(ans, "\n");
	}
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

static char* template() {
	string *ans = make_string(BUFSIZE);
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}

char* translate(double a, double b, int n, AST **funcs, char **fnames) {
	string *ans = make_string(BUFSIZE);
	identifiers_table *table = mktable();
	add_named_identifier(table, "a", a);
	add_named_identifier(table, "b", b);
	for(int i = 0; i < n; ++i) {
		find_identifiers(table, funcs[i]);
	}
	append(ans, gen_header());
	append(ans, "\n");
	append(ans, gen_rodata(table));
	append(ans, "\n");
	append(ans, gen_text(n, funcs, fnames, table));
	destroytable(table);
	char *retbuf = ans->buf;
	free(ans);
	return retbuf;
}
