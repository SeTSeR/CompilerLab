#include "symtab.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define eps 0.00001

enum {
	INITIAL_CAPACITY = 10,
	IDENTIFIER_LENGTH = 15
};

identifiers_table* mktable() {
	identifiers_table *table = calloc(1, sizeof(identifiers_table));
	table->size = 0;
	table->capacity = INITIAL_CAPACITY;
	table->identifiers = calloc(table->capacity, sizeof(identifier));
	return table;
}

void destroytable(identifiers_table *table) {
	for(int i = 0; i < table->size; ++i) free(table->identifiers[i].name);
	table->size = 0;
	table->capacity = 0;
	free(table->identifiers);
	free(table);
}

static identifier* _lookup(identifiers_table* table, double value) {
	if((table == NULL) || (table->size == 0)) return NULL;
	for(int i = 0; i < table->size; ++i) {
		if((table->identifiers[i].name[0] == 'c') && (fabs(table->identifiers[i].value - value) < eps)) return &(table->identifiers[i]);
	}
	return NULL;
}

char* lookup(identifiers_table* table, double value) {
	identifier *id = _lookup(table, value);
	if(id) return id->name;
	return NULL;
}

void add_identifier(identifiers_table* table, double value) {
	if(_lookup(table, value) == NULL) {
		if(table->size == table->capacity) {
			table->capacity <<= 1;
			table->identifiers = realloc(table->identifiers, table->capacity);
		}
		int num = table->size;
		char *name = calloc(IDENTIFIER_LENGTH, sizeof(char));
		snprintf(name, IDENTIFIER_LENGTH, "const%d", num);
		table->identifiers[table->size].name = name;
		table->identifiers[table->size].value = value;
		++table->size;
	}
}

void add_named_identifier(identifiers_table* table, char* name, double value) {
	if(table->size == table->capacity) {
		table->capacity <<= 1;
		table->identifiers = realloc(table->identifiers, table->capacity);
	}
	int lenname = strlen(name);
	char *newname = calloc(lenname + 1, sizeof(char));
	strncpy(newname, name, lenname);
	table->identifiers[table->size].name = newname;
	table->identifiers[table->size].value = value;
	++table->size;
}
