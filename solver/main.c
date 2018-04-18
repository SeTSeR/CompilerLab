#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dlfcn.h>

#include "analytic.h"

#define eps1 0.00001
#define eps2 0.00001

static inline void error(char* message) {
	fprintf(stderr, "%s\n", message);
	exit(EXIT_FAILURE);
}

static inline double min(double a, double b, double c) {
	if((a < b) && (a < c)) return a;
	if((b < a) && (b < c)) return b;
	return c;
}

static inline void swap(void* a, void* b) {
	void *tmp = a;
	a = b;
	b = tmp;
}

int main(int argc, char** argv) {
	bool debug = false;
	for(int i = 1; i < argc; ++i) {
		if((!strncmp(argv[i], "--help", 6)) || (!strncmp(argv[i], "-h", 2))) {
			puts("Command-line keys:");
			puts("--help or -h: prints this message");
			puts("--debug or -g: enables debugging output");
			return 0;
		}
		else if((!strncmp(argv[i], "--debug", 7)) || (!strncmp(argv[i], "-g", 2))) {
			debug = true;
		}
	}
	void *handle = dlopen("./libfunctions.so", RTLD_LAZY);
	if(!handle) error(dlerror());
	dlerror();
	double (*f)(double);
	double (*df)(double);
	double (*g)(double);
	double (*dg)(double);
	double (*h)(double);
	double (*dh)(double);
	double *a, *b;
	char* errmsg;
	f = (double (*)(double)) dlsym(handle, "f1");
	if(errmsg = dlerror()) error(errmsg);
	df = (double (*)(double)) dlsym(handle, "df1");
	if(errmsg = dlerror()) error(errmsg);
	g = (double (*)(double)) dlsym(handle, "f2");
	if(errmsg = dlerror()) error(errmsg);
	dg = (double (*)(double)) dlsym(handle, "df2");
	if(errmsg = dlerror()) error(errmsg);
	h = (double (*)(double)) dlsym(handle, "f3");
	if(errmsg = dlerror()) error(errmsg);
	dh = (double (*)(double)) dlsym(handle, "df3");
	if(errmsg = dlerror()) error(errmsg);
	a = (double*)dlsym(handle, "a");
	if(errmsg = dlerror()) error(errmsg);
	b = (double*)dlsym(handle, "b");
	if(errmsg = dlerror()) error(errmsg);
	double x1 = root(f, df, g, dg, *a, *b, eps1);
	double x2 = root(g, dg, h, dh, *a, *b, eps1);
	double x3 = root(h, dh, f, df, *a, *b, eps1);
	if(debug) {
		for(double x = *a; x < *b; x += 0.5) {
			printf("%f ", (*df)(x));
		}
		printf("\n");
	}
	if(debug){
		printf("Roots: %f %f %f\n", x1, x2, x3);
	}
	double answer = fabs(integrate(f, x1, x3, eps2) + integrate(h, x3, x2, eps2) + integrate(g, x2, x1, eps2));
	printf("The answer is: %f\n", answer);
	dlclose(handle);
	return 0;
}
