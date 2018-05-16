#include "test.h"
#include "optimizer.h"

int check_optimize1() {
	char toparse1[] = "2 2 3 + *";
	char toparse2[] = "10";
	AST *checked = parse(toparse1);
	perform_optimizations(checked);
	AST *checker = parse(toparse2);
	int answer = equal(checked, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(checked, 4, stderr);
	}
	destroy_tree(checked);
	destroy_tree(checker);
	return answer;
}

int check_optimize2() {
	char toparse1[] = "x 2 3 + *";
	char toparse2[] = "x 5 *";
	AST *checked = parse(toparse1);
	perform_optimizations(checked);
	AST *checker = parse(toparse2);
	int answer = equal(checked, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(checked, 4, stderr);
	}
	destroy_tree(checked);
	destroy_tree(checker);
	return answer;
}

int check_optimize3() {
	char toparse1[] = "0 x 3 + *";
	char toparse2[] = "0";
	AST *checked = parse(toparse1);
	perform_optimizations(checked);
	AST *checker = parse(toparse2);
	int answer = equal(checked, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(checked, 4, stderr);
	}
	destroy_tree(checked);
	destroy_tree(checker);
	return answer;
}

int check_optimize4() {
	char toparse1[] = "x 3 + sin x 3 + sin -";
	char toparse2[] = "0";
	AST *checked = parse(toparse1);
	perform_optimizations(checked);
	AST *checker = parse(toparse2);
	int answer = equal(checked, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(checked, 4, stderr);
	}
	destroy_tree(checked);
	destroy_tree(checker);
	return answer;
}

int check_optimize5() {
	char toparse1[] = "x 3 + sin x 3 + sin /";
	char toparse2[] = "1";
	AST *checked = parse(toparse1);
	perform_optimizations(checked);
	AST *checker = parse(toparse2);
	int answer = equal(checked, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(checked, 4, stderr);
	}
	destroy_tree(checked);
	destroy_tree(checker);
	return answer;
}
