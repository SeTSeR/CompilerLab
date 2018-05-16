#include "test.h"

int check_parse1() {
	char toparse[] = "2";
	AST *parsed = parse(toparse);
	AST *matching = create_tree();
	matching->type = NUMBER;
	matching->value = 2.0;
	int answer = equal(parsed, matching);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(matching, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(parsed, 4, stderr);
	}
	destroy_tree(matching);
	destroy_tree(parsed);
	return answer;
}

int check_parse2() {
	char toparse[] = "x";
	AST *parsed = parse(toparse);
	AST *matching = create_tree();
	matching->type = VARIABLE;
	int answer = equal(parsed, matching);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(matching, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(parsed, 4, stderr);
	}
	destroy_tree(matching);
	destroy_tree(parsed);
	return answer;
}

int check_parse3() {
	char toparse[] = "2 3 5 * +";
	AST *parsed = parse(toparse);
	AST *matching = create_tree();
	matching->type = OPERATOR;
	matching->op_type = PLUS;
	matching->first_param = create_tree();
	matching->second_param = create_tree();
	matching->first_param->type = NUMBER;
	matching->first_param->value = 2.0;
	matching->second_param->type = OPERATOR;
	matching->second_param->op_type = MULTIPLY;
	matching->second_param->first_param = create_tree();
	matching->second_param->second_param = create_tree();
	matching->second_param->first_param->type = NUMBER;
	matching->second_param->first_param->value = 3.0;
	matching->second_param->second_param->type = NUMBER;
	matching->second_param->second_param->value = 5.0;
	int answer = equal(parsed, matching);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(matching, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(parsed, 4, stderr);
	}
	destroy_tree(matching);
	destroy_tree(parsed);
	return answer;
}

int check_parse4() {
	char toparse[] = "2 x sin /";
	AST *parsed = parse(toparse);
	AST *matching = create_tree();
	matching->type = OPERATOR;
	matching->op_type = DIVIDE;
	matching->first_param = create_tree();
	matching->second_param = create_tree();
	matching->first_param->type = NUMBER;
	matching->first_param->value = 2.0;
	matching->second_param->type = OPERATOR;
	matching->second_param->op_type = SIN;
	matching->second_param->first_param = create_tree();
	matching->second_param->first_param->type = VARIABLE;
	int answer = equal(parsed, matching);
	if(answer == 0) {
		fprintf(stderr, "Test %d failed:\n", testnum);
		fprintf(stderr, "Incorrect tree parsing. Expected:\n");
		print_tree(matching, 4, stderr);
		fprintf(stderr, "\nGot:\n");
		print_tree(parsed, 4, stderr);
	}
	destroy_tree(matching);
	destroy_tree(parsed);
	return answer;
}
