#include "basic.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "codegen.c"

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

	instruction_list instructions = compile(prg);
	if (instructions.len == 0) {
		fprintf(stderr, "error: cannot generate instructions\n");
		exit(1);
	}
	codegen(instructions, file_path, detect_host_machine());

	return 0;
}