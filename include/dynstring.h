#include <stddef.h>

typedef struct vector {
	char* buf;
	size_t size;
	size_t capacity;
} string;

string *make_string(int);
string *from_cstring(char*);
void append(string*, char*);
void append_line(int, string*, char*);
