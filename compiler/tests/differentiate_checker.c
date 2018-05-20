#include "test.h"
#include "parser.h"
#include "differentiate.h"

int check_differentiate1() {
	char toparse1[] = "2";
	char toparse2[] = "0";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}

int check_differentiate2() {
	char toparse1[] = "x";
	char toparse2[] = "1";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}

int check_differentiate3() {
	char toparse1[] = "2 3 +";
	char toparse2[] = "0 0 +";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}

int check_differentiate4() {
	char toparse1[] = "2 x *";
	char toparse2[] = "0 x * 2 1 * +";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}

int check_differentiate5() {
	char toparse1[] = "2 x tan *";
	char toparse2[] = "0 x tan * 2 1 x cos x cos * / * +";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}

int check_differentiate6() {
	char toparse1[] = "x 2 ^";
	char toparse2[] = "x 2 ^ 0 x ln * 2 1 x / * + *";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}

int check_differentiate7() {
	char toparse1[] = "x sin x cos ^";
	char toparse2[] = "x sin x cos ^ 0 x sin 1 * - x sin ln * x cos x cos 1 * x sin / * + *";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}

int check_differentiate8() {
	char toparse1[] = "1 x - 3 /";
	char toparse2[] = "0 1 - 3 * 1 x - 0 * - 3 3 * /";
	AST *source = parse(toparse1);
	AST *deriv = derivative(source);
	AST *checker = parse(toparse2);
	int answer = equal(deriv, checker);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect derivative taking. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}
