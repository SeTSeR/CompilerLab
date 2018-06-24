#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dlfcn.h>

#include "analytic.h"

#define eps2 0.00057735026 // 0.001/sqrt(3)

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

static inline double dist(double x, double y, double z) {
	return x * x + y * y + z * z;
}

static inline void print_help() {
	puts("Command-line keys:");
	puts("  --help or -h: prints this message");
	puts("  --roots or -r: enables roots printing");
	puts("  --debug or -g: enables debugging info");
	puts("  --iterations or -i: enables printing count of iterations");
	puts("  --libfile or -f <filename>: set path to library file");
	exit(EXIT_SUCCESS);
}

int main(int argc, char** argv) {
	bool debug = false;
	bool roots = false;
	bool iterations = false;
	char libfilename[128] = "./libfunctions.so";
	for(int i = 1; i < argc; ++i) {
		if(argv[i][1] == '-') {
			if(!strncmp(argv[i], "--help", 6)) {
				print_help();
			}
			else if(!strncmp(argv[i], "--debug", 7)) {
				debug = true;
				roots = true;
				iterations = true;
			}
			else if(!strncmp(argv[i], "--roots", 7)) {
				roots = true;
			}
			else if(!strncmp(argv[i], "--iterations", 12)) {
				iterations = true;
			}
			else if(!strncmp(argv[i], "--libfile", 9)) {
				int filelen = strlen(argv[i + 1]);
				strncpy(libfilename, argv[i + 1], filelen);
			}
		}
		else if(argv[i][0] == '-') {
			if(strchr(argv[i], 'h')) {
				print_help();
			}
			if(strchr(argv[i], 'g')) {
				debug = true;
				roots = true;
				iterations = true;
			}
			else if(strchr(argv[i], 'f')) {
				int filelen = strlen(argv[i + 1]);
				strncpy(libfilename, argv[i + 1], filelen);
			}
			else {
				if(strchr(argv[i], 'r')) {
					roots = true;
				}
				if(strchr(argv[i], 'i')) {
					iterations = true;
				}
			}
		}
	}
	void *handle = dlopen(libfilename, RTLD_LAZY);
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
	double eps1 = 0.001;
	double x1 = root(f, df, g, dg, *a, *b, eps1, iterations);
	double x2 = root(g, dg, h, dh, *a, *b, eps1, iterations);
	double x3 = root(h, dh, f, df, *a, *b, eps1, iterations);
	double neweps1 = eps1 / dist(g(x1) - f(x1), h(x2) - g(x2), f(x3) - h(x3));
	if(debug) {
		printf("f(x):\n");
		for(double x = *a; x <= *b; x += 0.5) {
			printf("%f ", (*f)(x));
		}
		printf("\ng(x):\n");
		for(double x = *a; x <= *b; x += 0.5) {
			printf("%f ", (*g)(x));
		}
		printf("\nh(x):\n");
		for(double x = *a; x <= *b; x += 0.5) {
			printf("%f ", (*h)(x));
		}
		printf("\ndf(x):\n");
		for(double x = *a; x <= *b; x += 0.5) {
			printf("%f ", (*df)(x));
		}
		printf("\ndg(x):\n");
		for(double x = *a; x <= *b; x += 0.5) {
			printf("%f ", (*dg)(x));
		}
		printf("\ndh(x):\n");
		for(double x = *a; x <= *b; x += 0.5) {
			printf("%f ", (*dh)(x));
		}
		printf("\n");
	}
	if(neweps1 < eps1) {
		eps1 = neweps1;
		double x1 = root(f, df, g, dg, *a, *b, eps1, iterations);
		double x2 = root(g, dg, h, dh, *a, *b, eps1, iterations);
		double x3 = root(h, dh, f, df, *a, *b, eps1, iterations);
	}
	if(roots){
		printf("Roots: %f %f %f\n", x1, x2, x3);
	}
	double answer = fabs(integrate(f, x1, x3, eps2) + integrate(h, x3, x2, eps2) + integrate(g, x2, x1, eps2));
	printf("The answer is: %f\n", answer);
	dlclose(handle);
	return 0;
}
