#include "test.h"

#include <math.h>

double zero(double x) {
	return 0.0;
}

double two(double x) {
	return 2.0;
}

double cube(double x) {
	return x*x*x;
}

double dcube(double x) {
	return 3*x*x;
}

double hardfun(double x) {
	return fmin(pow(2, x), x * x);
}
