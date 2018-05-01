#include "test.h"

#include <stdio.h>

int main(int argc, char** argv) {
	int passedcount = 0;
	passedcount += check_solve_sinus();
	++testnum;
	passedcount += check_solve_cube();
	++testnum;
	passedcount += check_solve_sincube();
	++testnum;
	printf("Solve tests: %d of %d tests passsed\n", passedcount, testnum - 1);
	return 0;
}
