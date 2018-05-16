#include "test.h"
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
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
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
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
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
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
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
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
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
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(checker, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(deriv, 4, stderr);
	}
	destroy_tree(deriv);
	destroy_tree(source);
	destroy_tree(checker);
	return answer;
}
