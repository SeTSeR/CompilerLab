#include "test.h"

#include <stdio.h>

int testnum = 1;

int main(void) {
	int passedcount = 0;
	passedcount += check_solve_sinus();
	++testnum;
	passedcount += check_solve_cube();
	++testnum;
	passedcount += check_solve_sincube();
	++testnum;
	passedcount += check_integrate_sinus();
	++testnum;
	passedcount += check_integrate_cube();
	++testnum;
	printf("Analytic tests: %d of %d tests passsed\n", passedcount, testnum - 1);
	return 0;
}
