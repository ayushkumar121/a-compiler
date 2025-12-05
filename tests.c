// Globals
int error_count = 0;

#include "basic.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "codegen.c"

void test_type_parsing(void) {
	lexer lex = lexer_from_string(sv("const *int"));
	type t = parse_type(&lex);
	ASSERT(string_eq(sv("const *int"), type_to_string(t)));
}

void test_infix_expr_parsing(void) {
	lexer lex = lexer_from_string(sv("2*3+1"));
	expression ex = parse_expression(&lex);
	ASSERT(string_eq(sv("(+ (* 2 3) 1)"), expression_to_string(ex)));

	lex = lexer_from_string(sv("f . g . h"));
	ex = parse_expression(&lex);
	ASSERT(string_eq(sv("(. f (. g h))"), expression_to_string(ex)));
}

void test_prefix_expr_parsing(void) {
	lexer lex = lexer_from_string(sv("5**b"));
	expression ex = parse_expression(&lex);
	ASSERT(string_eq(sv("(* 5 (* b))"), expression_to_string(ex)));

	lex = lexer_from_string(sv("1+(2+3)"));
	ex = parse_expression(&lex);
	ASSERT(string_eq(sv("(+ 1 (+ 2 3))"), expression_to_string(ex)));
}

void test_post_expr_parsing(void) {
	lexer lex = lexer_from_string(sv("a?.b"));
	expression ex = parse_expression(&lex);
	ASSERT(string_eq(sv("(. (? a) b)"), expression_to_string(ex)));

	lex = lexer_from_string(sv("a?.*b"));
	ex = parse_expression(&lex);
	ASSERT(string_eq(sv("(. (? a) (* b))"), expression_to_string(ex)));

	lex = lexer_from_string(sv("x[0][1]"));
	ex = parse_expression(&lex);
	ASSERT(string_eq(sv("([] ([] x 0) 1)"), expression_to_string(ex)));
}

void test_compiler_001(void) {
	int status = system("./tinyc ./examples/001.tc && ./examples/001");
    ASSERT(WIFEXITED(status));
    ASSERT(WEXITSTATUS(status) == 40);
}

void test_compiler_002(void) {
	int status = system("./tinyc ./examples/002-strings.tc && ./examples/002-strings");
    ASSERT(WIFEXITED(status));
    ASSERT(WEXITSTATUS(status) == 205);
}

void test_compiler_003(void) {
	int status = system("./tinyc ./examples/003-arrays.tc && ./examples/003-arrays");
    ASSERT(WIFEXITED(status));
    ASSERT(WEXITSTATUS(status) == 110);
}
int main(void) {
	test_type_parsing();
	test_infix_expr_parsing();
	test_prefix_expr_parsing();
	test_post_expr_parsing();
	test_compiler_001();
	test_compiler_002();
	test_compiler_003();

	fprintf(stderr, "info: all test passed\n");
}