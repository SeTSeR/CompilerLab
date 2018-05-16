#include "ast.h"
#include "parser.h"

#include <stdio.h>

extern int testnum;

int check_parse1();
int check_parse2();
int check_parse3();
int check_parse4();

int check_differentiate1();
int check_differentiate2();
int check_differentiate3();
int check_differentiate4();
int check_differentiate5();

int check_optimize1();
int check_optimize2();
int check_optimize3();
int check_optimize4();
int check_optimize5();

int check_symtab_add();
int check_symtab_add_named();
int check_symtab_lookup_found();
int check_symtab_lookup_not_found();
