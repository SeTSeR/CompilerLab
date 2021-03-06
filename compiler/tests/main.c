#include "test.h"

int testnum = 1;

int main(void) {
	int passedcount = 0;
	int prevtests = 0;
	passedcount += check_parse1();
	++testnum;
	passedcount += check_parse2();
	++testnum;
	passedcount += check_parse3();
	++testnum;
	passedcount += check_parse4();
	++testnum;
	passedcount += check_parse5();
	++testnum;
	passedcount += check_parse6();
	++testnum;
	passedcount += check_parse7();
	++testnum;
	printf("Parser tests: %d of %d tests passed\n", passedcount - prevtests, testnum - prevtests - 1);
	prevtests = testnum - 1;
	passedcount += check_differentiate1();
	++testnum;
	passedcount += check_differentiate2();
	++testnum;
	passedcount += check_differentiate3();
	++testnum;
	passedcount += check_differentiate4();
	++testnum;
	passedcount += check_differentiate5();
	++testnum;
	passedcount += check_differentiate6();
	++testnum;
	passedcount += check_differentiate7();
	++testnum;
	passedcount += check_differentiate8();
	++testnum;
	printf("Differentiate tests: %d of %d tests passed\n", passedcount - prevtests, testnum - prevtests - 1);
	prevtests = testnum - 1;
	passedcount += check_optimize1();
	++testnum;
	passedcount += check_optimize2();
	++testnum;
	passedcount += check_optimize3();
	++testnum;
	passedcount += check_optimize4();
	++testnum;
	passedcount += check_optimize5();
	++testnum;
	passedcount += check_optimize6();
	++testnum;
	passedcount += check_optimize7();
	++testnum;
	passedcount += check_optimize8();
	++testnum;
	passedcount += check_optimize9();
	++testnum;
	passedcount += check_optimize10();
	++testnum;
	printf("Optimizer tests: %d of %d tests passed\n", passedcount - prevtests, testnum - prevtests - 1);
	prevtests = testnum - 1;
	passedcount += check_symtab_add();
	++testnum;
	passedcount += check_symtab_add_named();
	++testnum;
	passedcount += check_symtab_lookup_found();
	++testnum;
	passedcount += check_symtab_lookup_not_found();
	++testnum;
	printf("Symtab tests: %d of %d tests passed\n", passedcount - prevtests, testnum - prevtests - 1);
	prevtests = testnum - 1;
	printf("Summary: %d of %d tests passed\n", passedcount, testnum - 1);
	return 0;
}
