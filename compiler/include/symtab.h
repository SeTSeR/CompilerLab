#include <stddef.h>

typedef struct {
	char* name;
	double value;
} identifier;

typedef struct {
	identifier* identifiers;
	size_t size;
	size_t capacity;
} identifiers_table;

identifiers_table* mktable();
void destroytable(identifiers_table*);
char* lookup(identifiers_table*, double);
void add_identifier(identifiers_table*, double);
void add_named_identifier(identifiers_table*, char*, double);
