double integrate(double (*f)(double), double a, double b, double eps);
double solve_newton(double (*f)(double), double (*df)(double), double (*d2f)(double), double a, double b, double eps);
double solve_binsearch(double (*f)(double), double a, double b, double eps);
