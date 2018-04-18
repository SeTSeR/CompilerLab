#include <math.h>
#include <stdio.h>

#include "analytic.h"

double integrate(double (*f)(double), double a, double b, double eps) {
	int n = 1000;
	double sum1 = 0, sum2 = 0;
	double step = (b - a) / n;
	for(int i = 0; i < n; ++i) {
		sum2 += ((*f)(a + i * step) + 4*(*f)(a + (i + 0.5) * step) + (*f)(a + (i + 1) * step))*step/6;
	}
	while(fabs(sum2 - sum1) >= eps) {
		sum1 = sum2;
		sum2 = 0;
		n *= 2;
		step = (b - a) / n;
		for(int i = 0; i < n; ++i) {
			sum2 += ((*f)(a + i * step) + 4*(*f)(a + (i + 0.5) * step) + (*f)(a + (i + 1) * step))*step/6;
		}
	}
	return sum2;
}

double solve_newton(double (*f)(double), double (*df)(double), double (*d2f)(double), double a, double b, double eps) {
	while((b - a) > (2 * eps)) {
		if((*f)(a) * (*d2f)(a) < 0) 
			a = a - ((*f)(a))*(a - b)/((*f)(a) - (*f)(b));
		else
			a = a - (*f)(a)/((*df)(a));
		if((*f)(b) * (*d2f)(a) < 0)
			b = b - ((*f)(b))*(b - a)/((*f)(b) - (*f)(a));
		else
			b = b - (*f)(b)/((*df)(b));
	}
	return a + (b - a) / 2;
}

double solve_binsearch(double (*f)(double), double a, double b, double eps) {
	while((b - a) >= eps) {
		if((*f)(a + eps/2) * (*f)(a - eps/2) < 0) break;
		double mid = a + (b - a) / 2;
		if((*f)(a) * (*f)(mid) < 0) b = mid;
		else a = mid;
	}
	return a;
}
