#include <math.h>
#include <stdbool.h>
#include <stdio.h>

#include "analytic.h"
#include "test.h"

#define eps 0.0001


double zero(double x) {
	return 0;
}

double cube(double x) {
	return x*x*x;
}

double dcube(double x) {
	return 3*x*x;
}

double cubeminsin(double x) {
	return cube(x) - sin(x);
}

int check_solve_sinus() {
	double rt = root(sin, cos, zero, zero, 0, 1, eps, false);
	if(fabs(sin(rt)) >= eps) {
		fprintf(stderr, "Test %d failed: sin(%lf) != 0\n", testnum, rt);
		return 0;
	}
	return 1;
}

int check_solve_cube() {
	double rt = root(cube, dcube, zero, zero, 0, 3, eps, false);
	if(fabs(cube(rt)) >= eps) {
		fprintf(stderr, "Test %d failed: %lf^3 != 0\n", testnum, rt);
		return 0;
	}
	return 1;
}

int check_solve_sincube() {
	double rt = root(cube, dcube, sin, cos, 0.1, 1, eps, false);
	if(fabs(cubeminsin(rt)) >= eps) {
		fprintf(stderr, "Test %d failed: sin(%lf) != %lf^3\n", testnum, rt, rt);
		return 0;
	}
	return 1;
}
