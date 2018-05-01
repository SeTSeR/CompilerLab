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
		fprintf(stderr, "Error in test %d: integral of sin(x) from %lf to %lf != %lf", testnum, 0.0, M_PI, integral);
		return 0;
	}
	return 1;
}
