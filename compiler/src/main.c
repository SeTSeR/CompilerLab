#include "codegen.h"
#include "differentiate.h"
#include "parser.h"
#include "optimizer.h"

#include <pthread.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *get_trees(void *param) {
	AST** trees = calloc(2, sizeof(AST*));
	trees[0] = parse((char*)param);
	optimize(trees[0]);
	trees[1] = derivative(trees[0]);
	optimize(trees[1]);
	pthread_exit(trees);
}

int main(int argc, char** argv) {
	pthread_t tid[3];
	pthread_attr_t attr;
	if(argc < 3) return -1;
	FILE *in = fopen(argv[1], "rt");
	pthread_attr_init(&attr);
	char buf[3][256];
	fgets(buf[0], 256, in);
	double a, b;
	sscanf(buf[0], "%lf %lf", &a, &b);
	AST* trees[6];
	for(size_t i = 0; i < 3; ++i) {
		fgets(buf[i], 256, in);
		pthread_create(&tid[i], &attr, get_trees, buf[i]);
	}
	void *res_trees;
	for(int i = 0; i < 3; ++i) {
		pthread_join(tid[i], &res_trees);
		if(res_trees) {
			trees[i] = ((AST**)res_trees)[0];
			trees[i + 3] = ((AST**)res_trees)[1];
			free(res_trees);
		}
	}
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
