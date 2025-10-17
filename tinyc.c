#include "basic.c"
#include "lexer.c"
#include "parser.c"

int main(int argc, char** argv) {
	Args args = {1, argc, argv};

	string file_path = args_next(&args);
	if (file_path.len == 0) {
		fprintf(stderr, "ERROR: no file provided\n");
		exit(1);
	}

	lexer lex = lexer_from_file(file_path);
	parse_program(&lex);

	return 0;
}