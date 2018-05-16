#include "codegen.h"
#include "dynstring.h"
#include "parser.h"
#include "symtab.h"

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define eps 0.000001

enum {
	BUFSIZE = 256,
	LABEL_LEVEL = 0,
	FUNC_LEVEL = 4
};

bool check_predefined = false;
double predefined[7] = {0.0, 1.0, M_PI, M_LOG2E, M_LN10/M_LN2, M_LN2/M_LN10, M_LN2};
char* commands[7] = {"fldz", "fld1", "fldpi", "fldl2e", "fldl2t", "fldlg2", "fldln2"};

inline static int is_predefined(double value) {
	for(size_t i = 0; i < 7; ++i) if(fabs(value - predefined[i]) < eps) return i;
	return -1;
}

static void find_identifiers(identifiers_table *table, AST *tree) {
	if(tree) {
		switch(tree->type) {
			case NUMBER:
				if((!check_predefined) || (is_predefined(tree->value) == -1)) add_identifier(table, tree->value);
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
	return destroy_string(ans);
}

static char* gen_prolog() {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "push rbp");
	append_line(FUNC_LEVEL, ans, "mov rbp, rsp");
	append_line(FUNC_LEVEL, ans, "sub rsp, 8");
	append_line(FUNC_LEVEL, ans, "movsd qword[rsp], xmm0");
	return destroy_string(ans);
}

static char* gen_epilog() {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "movsd xmm0, qword[rsp]");
	append_line(FUNC_LEVEL, ans, "add rsp, 16");
	append_line(FUNC_LEVEL, ans, "pop rbp");
	return destroy_string(ans);
}

static char* gen_node(AST* node, identifiers_table *table) {
	string *ans = make_string(BUFSIZE);
	char command[BUFSIZE];
	int pred = -1;
    char *leftnode, *rightnode;
	switch(node->type) {
		case NUMBER:
			if(check_predefined) {
				pred = is_predefined(node->value);
				if((0 <= pred) && (pred < 7)) snprintf(command, 128, "%s", commands[pred]);
				else snprintf(command, BUFSIZE, "fld qword[%s]", lookup(table, node->value));
				append_line(FUNC_LEVEL, ans, command);
			}
			else {
				snprintf(command, BUFSIZE, "mov rax, qword[%s]", lookup(table, node->value));
				append_line(FUNC_LEVEL, ans, command);
				append_line(FUNC_LEVEL, ans, "push rax");
			}
			break;
		case VARIABLE:
			append_line(FUNC_LEVEL, ans, "push qword[rbp - 8]");
			break;
		case OPERATOR:
			switch(node->op_type) {
				case PLUS:
                    leftnode = gen_node(node->first_param, table);
                    rightnode = gen_node(node->second_param, table);
					append(ans, leftnode);
					append(ans, rightnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp + 8]");
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "faddp");
					append_line(FUNC_LEVEL, ans, "add rsp, 8");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
                    free(rightnode);
					break;
				case MINUS:
                    leftnode = gen_node(node->first_param, table);
                    rightnode = gen_node(node->second_param, table);
					append(ans, leftnode);
					append(ans, rightnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp + 8]");
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "fsubp");
					append_line(FUNC_LEVEL, ans, "add rsp, 8");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
                    free(rightnode);
					break;
				case MULTIPLY:
                    leftnode = gen_node(node->first_param, table);
                    rightnode = gen_node(node->second_param, table);
					append(ans, leftnode);
					append(ans, rightnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp + 8]");
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "fmulp");
					append_line(FUNC_LEVEL, ans, "add rsp, 8");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
                    free(rightnode);
					break;
				case DIVIDE:
                    leftnode = gen_node(node->first_param, table);
                    rightnode = gen_node(node->second_param, table);
					append(ans, leftnode);
					append(ans, rightnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp + 8]");
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "fdivp");
					append_line(FUNC_LEVEL, ans, "add rsp, 8");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
                    free(rightnode);
					break;
				case SIN:
                    leftnode = gen_node(node->first_param, table);
					append(ans, leftnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "fsin");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
					break;
				case COS:
                    leftnode = gen_node(node->first_param, table);
					append(ans, leftnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "fcos");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
					break;
				case TAN:
                    leftnode = gen_node(node->first_param, table);
					append(ans, leftnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "fptan");
					append_line(FUNC_LEVEL, ans, "fstp st0");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
					break;
				case CTG:
                    leftnode = gen_node(node->first_param, table);
					append(ans, leftnode);
					append_line(FUNC_LEVEL, ans, "fld qword[rsp]");
					append_line(FUNC_LEVEL, ans, "fptan");
					append_line(FUNC_LEVEL, ans, "fdivp");
					append_line(FUNC_LEVEL, ans, "fstp qword[rsp]");
                    free(leftnode);
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
	return destroy_string(ans);
}

static char* gen_function(char* fname, AST* func, identifiers_table *table) {
	string *ans = from_cstring(fname);
	append(ans, ":\n");
    char *prolog = gen_prolog();
    char *node = gen_node(func, table);
    char *epilog = gen_epilog();
	append(ans, prolog);
	append(ans, node);
	append(ans, epilog);
	append_line(FUNC_LEVEL, ans, "ret");
    free(prolog);
    free(node);
    free(epilog);
	return destroy_string(ans);
}

static char* gen_rodata(identifiers_table *table) {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "section .rodata");
	for(size_t i = 0; i < table->size; ++i) {
		char idline[128];
		snprintf(idline, 128, "%s dq %lf", table->identifiers[i].name, table->identifiers[i].value);
		append_line(FUNC_LEVEL, ans, idline);
	}
	return destroy_string(ans);
}

static char* gen_text(size_t n, AST **funcs, char **fnames, identifiers_table *table) {
	string *ans = make_string(BUFSIZE);
	append_line(FUNC_LEVEL, ans, "section .text");
	for(size_t i = 0; i < n; ++i) {
        char *function = gen_function(fnames[i], funcs[i], table);
		append(ans, function);
		append(ans, "\n");
        free(function);
	}
	return destroy_string(ans);
}

char* translate(double a, double b, size_t n, AST **funcs, char **fnames) {
	string *ans = make_string(BUFSIZE);
	identifiers_table *table = create_table();
	add_named_identifier(table, "a", a);
	add_named_identifier(table, "b", b);
	for(size_t i = 0; i < n; ++i) {
		find_identifiers(table, funcs[i]);
	}
    char *header = gen_header();
    char *rodata = gen_rodata(table);
    char *text = gen_text(n, funcs, fnames, table);
	append(ans, header);
	append(ans, "\n");
	append(ans, rodata);
	append(ans, "\n");
	append(ans, text);
	destroy_table(table);
    free(header);
    free(rodata);
    free(text);
	return destroy_string(ans);
}
