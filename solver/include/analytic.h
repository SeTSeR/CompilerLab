#include <stdbool.h>

double integrate(double (*)(double), double, double, double);
double root(double (*f)(double), double (*)(double), double (*)(double), double (*)(double), double, double, double, bool);
