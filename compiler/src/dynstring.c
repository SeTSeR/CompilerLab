#include "dynstring.h"

#include <stdlib.h>
#include <string.h>

static inline size_t max(size_t a, size_t b) {
	return a > b ? a : b;
}

string *make_string(int initial_capacity) {
	string *new_str = calloc(1, sizeof(string));
	new_str->size = 1; //null terminator is already present;
	new_str->capacity = initial_capacity;
	new_str->buf = calloc(new_str->capacity, sizeof(char));
	return new_str;
}

string *from_cstring(char* str) {
	int len = strlen(str);
	string *new_str = calloc(1, sizeof(string));
	new_str->size = len + 1;
	new_str->capacity = len + 1;
	new_str->buf = calloc(new_str->capacity, sizeof(char));
	strncpy(new_str->buf, str, len);
	return new_str;
}

char* destroy_string(string *str) {
	char* buf = str->buf;
	free(str);
	return buf;
}

void append(string* str, char* seq) {
	int seqlen = strlen(seq);
	if(str->size + seqlen > str->capacity) {
		str->capacity = max(str->capacity * 2, str->size + seqlen);
		str->buf = realloc(str->buf, str->capacity * sizeof(char));
	}
	strncpy(str->buf + str->size - 1, seq, seqlen);
	str->size += seqlen;
	str->buf[str->size] = 0;
}

static void append_before(string* str, char* seq) {
	int seqlen = strlen(seq);
	if(str->size + seqlen > str->capacity) {
		str->capacity = max(str->capacity * 2, str->size + seqlen);
		str->buf = realloc(str->buf, str->capacity * sizeof(char));
	}
	memmove(str->buf + seqlen, str->buf, str->size);
	strncpy(str->buf, seq, seqlen);
	str->size += seqlen;
	str->buf[str->size] = 0;
}

void append_line(int level, string* str, char* seq) {
	char spaces[256] = "";
	memset(spaces, ' ', level);
	string *str1 = from_cstring(seq);
	append_before(str1, spaces);
	append(str, str1->buf);
	append(str, "\n");
	free(str1->buf);
	free(str1);
}
