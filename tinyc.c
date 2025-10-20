#include "basic.c"
#include "lexer.c"
#include "parser.c"

int main(int argc, char** argv) {
	Args args = {1, argc, argv};

	string file_path = args_next(&args);
	if (file_path.len == 0) {
		fprintf(stderr, "error: no file provided\n");
		exit(1);
	}
	lexer lex = lexer_from_file(file_path);
	program prg = parse_program(&lex);
	if (prg.errors > 0) {
		fprintf(stderr, "error: %d error encountered\n", prg.errors);
		exit(1);
	}
	symbol_table symbols = build_symbol_table(prg);
	if (symbols.len == 0) {
		fprintf(stderr, "error: invalid program\n");
		exit(1);
	}
	print_symbol_table(symbols);

	return 0;
}