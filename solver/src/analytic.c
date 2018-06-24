#include <math.h>
#include <stdio.h>

#include "analytic.h"

double integrate(double (*f)(double), double a, double b, double eps) {
	int n = 1000;
	double sum1 = 0, sum2 = 0;
	double step = (b - a) / n;
	double p = 1.0/15.0;
	sum2 = ((*f)(a) + (*f)(b));
	for(int i = 1; i <= n/2 - 1; ++i) {
		sum2 += 2*(*f)(a + 2 * i * step);
	}
	for(int i = 1; i <= n/2; ++i) {
		sum2 += 4*(*f)(a + (2 * i - 1) * step);
	}
	sum2 *= step/3;
	while(fabs(sum2 - sum1)*p >= eps) {
		sum1 = sum2;
		sum2 /= 2;
		double addend = 0;
		n *= 2;
		step = (b - a) / n;
		for(int i = 1; i <= n/4; ++i) {
			addend += 2 * (*f)(a + (4 * i - 1) * step) - (*f)(a + (4 * i - 2) * step) + 2 * (*f)(a + (4 * i - 3) * step);
		}
		sum2 += addend*step/1.5;
	}
	return sum2;
}

double (*givenf)(double);
double (*giveng)(double);
double (*givendf)(double);
double (*givendg)(double);

inline static double fming(double x) {
	return (*givenf)(x) - (*giveng)(x);
}

inline static double dfmindg(double x) {
	return (*givendf)(x) - (*givendg)(x);
}

#ifdef SOLVE_BINARY
static double solve(double (*f)(double), double(*df)(double), double a, double b, double eps, bool print_iterations) {
	int iterations = 0;
	while((b - a) >= eps) {
		if((*f)(a + eps/2) * (*f)(a - eps/2) < 0) break;
		double mid = a + (b - a) / 2;
		if((*f)(a) * (*f)(mid) < 0) b = mid;
		else a = mid;
		++iterations;
	}
	if(print_iterations) printf("Finding solution took %d iterations\n", iterations);
	return a;
}
#else
static double solve(double (*f)(double), double (*df)(double), double a, double b, double eps, bool print_iterations) {
	int iterations = 0;
	while((b - a) > (2 * eps)) {
		double sign2 = 0.5*(*f)(a + b) - (*f)(0.5*(a + b));
		if((*f)(a) * sign2 < 0)  {
			a = a - ((*f)(a))*(a - b)/((*f)(a) - (*f)(b));
			b = b - (*f)(b)/((*df)(b));
		}
		else {
			a = a - (*f)(a)/((*df)(a));
			b = b - ((*f)(b))*(b - a)/((*f)(b) - (*f)(a));
		}
		sign2 = (*df)(b + eps) - (*df)(b);
		++iterations;
	}
	if(print_iterations) printf("Finding solution took %d iterations\n", iterations);
	return a + (b - a) / 2;
}
#endif

double root(double (*f)(double), double (*df)(double),
			double (*g)(double), double (*dg)(double),
			double a, double b, double eps, bool print_iterations) {
	givenf = f, giveng = g, givendf = df, givendg = dg;
	return solve(fming, dfmindg, a, b, eps, print_iterations);
}
