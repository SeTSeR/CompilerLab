#include "analytic.h"
#include "test.h"

#include <math.h>
#include <stdio.h>

#define eps 0.0001

int check_integrate_sinus() {
	double integral = integrate(sin, 0, M_PI, eps);
	if(fabs(integral - 2.0) >= eps) {
		fprintf(stderr, "Error in test %d: integral of sin(x) from %lf to %lf != %lf", testnum, 0.0, M_PI, integral);
		return 0;
	}
	return 1;
}

int check_integrate_cube() {
	double integral = integrate(cube, 0, 3, eps);
	if(fabs(integral - 20.25) >= eps) {
		fprintf(stderr, "Error in test %d: integral of x^3 from %lf to %lf != %lf", testnum, 0.0, 3.0, integral);
		return 0;
	}
	return 1;
}

int check_integrate_hardfun() {
	double integral = integrate(hardfun, 0, 5, eps);
	double checker = 23 + 12*M_LOG2E;
	if(fabs(integral - checker) >= eps) {
		fprintf(stderr, "Error in test %d: integral of min(2^x, x^2) from %lf to %lf != %lf", testnum, 0.0, 5.0, integral);
		return 0;
	}
	return 1;
}
