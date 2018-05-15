#include "test.h"

#include <stdio.h>

int testnum = 1;

int main(void) {
	int passedcount = 0;
	passedcount += check_parse1();
	++testnum;
	passedcount += check_parse2();
	++testnum;
	printf("Parser tests: %d of %d tests passed\n", passedcount, testnum - 1);
	return 0;
}
