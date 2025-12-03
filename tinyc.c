int error_count;

#include "basic.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "simulator.c"
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
	intermediate_representation ir = compile(prg);
	if (error_count > 0) {
		fprintf(stderr, "error: %d errors occured during compilation\n", error_count);
		exit(1);
	}
	simulate(ir);
	codegen(ir, file_path, detect_host_machine());

	return 0;
}