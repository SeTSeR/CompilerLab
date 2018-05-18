#include "test.h"

#include <math.h>

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

double hardfun(double x) {
	return fmin(pow(2, x), x * x);
}
