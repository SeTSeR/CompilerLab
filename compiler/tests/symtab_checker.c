#include "test.h"
#include "symtab.h"

#include <string.h>

int check_symtab_add() {
	identifiers_table *table = create_table();
	int answer = 1;
	int errno = 0;
	add_identifier(table, 0.0);
	if(table->size < 1) 
		errno = 1;
	if(strncmp(table->identifiers[0].name, "const0", 6)) 
		errno = 2;
	if(errno) {
		answer = 0;
		fprintf(stderr, "Failed test %d:\n", testnum);
		switch(errno) {
			case 1:
				fprintf(stderr, "Element was not added\n");
				break;
			case 2:
				fprintf(stderr, "Element was added, but with incorrect name: %s\n", table->identifiers[0].name);
				break;
			default:
				break;
		}
	}
	destroy_table(table);
	return answer;
}

int check_symtab_add_named() {
	identifiers_table *table = create_table();
	int answer = 1;
	int errno = 0;
	add_named_identifier(table, "hello", 0.0);
	if(table->size < 1) 
		errno = 1;
	if(strncmp(table->identifiers[0].name, "hello", 5)) 
		errno = 2;
	if(errno) {
		answer = 0;
		fprintf(stderr, "Test %d failed:\n", testnum);
		switch(errno) {
			case 1:
				fprintf(stderr, "Element was not added\n");
				break;
			case 2:
				fprintf(stderr, "Element was added, but with incorrect name: %s\n", table->identifiers[0].name);
				break;
			default:
				break;
		}
	}
	destroy_table(table);
	return 1;
}

int check_symtab_lookup_found() {
	identifiers_table *table = create_table();
	int answer = 1;
	add_named_identifier(table, "hello", 0.0);
	add_identifier(table, 3.0);
	answer = lookup(table, 3.0) != NULL;
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Value %lf was not found though it was added\n", 3.0);
	}
	destroy_table(table);
	return answer;
}

int check_symtab_lookup_not_found() {
	identifiers_table *table = create_table();
	int answer = 1;
	add_named_identifier(table, "hello", 0.0);
	add_identifier(table, 3.0);
	answer = lookup(table, 2.0) == NULL;
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Value %lf was not found though it was not added\n", 2.0);
	}
	destroy_table(table);
	return answer;
}
