#include "codegen.h"
#include "differentiate.h"
#include "parser.h"
#include "optimizer.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv) {
	if(argc < 3) return -1;
	FILE *in = fopen(argv[1], "rt");
	char buf[256];
	fgets(buf, 256, in);
	double a, b;
	sscanf(buf, "%lf %lf", &a, &b);
	AST* trees[6];
	for(size_t i = 0; i < 3; ++i) {
		fgets(buf, 256, in);
		trees[i] = parse(buf);
		trees[i + 3] = derivative(trees[i]);
	}
	for(size_t i = 0; i < 6; ++i) perform_optimizations(trees[i]);
	char* names[6] = {"f1", "f2", "f3", "df1", "df2", "df3"};
	char* code = translate(a, b, 6, trees, names);
	FILE *out = fopen(argv[2], "wt");
	fputs(code, out);
	fclose(out);
    fclose(in);
    free(code);
    for(size_t i = 0; i < 6; ++i) destroy_tree(trees[i]);
	return 0;
}
