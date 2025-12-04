int error_count;

#include "basic.c"
#include "lexer.c"
#include "parser.c"
#include "compiler.c"
#include "simulator.c"
#include "codegen.c"

typedef enum {
	CMD_RUN,
	CMD_BUILD
} command_type;

int main(int argc, char** argv) {
	Args args = {1, argc, argv};

	command_type command;
	string command_str = args_next(&args);
	if (string_eq(command_str, sv("run"))) {
		command = CMD_RUN;
	} else if (string_eq(command_str, sv("build")))  {
		command = CMD_BUILD;
	} else {
		fprintf(stderr, "error: unknown command "sfmt"\n", sarg(command_str));
		exit(1);
	}

	string file_path = args_next(&args);
	if (file_path.len == 0) {
		fprintf(stderr, "error: no file provided\n");
		exit(1);
	}

	if (!file_exists(file_path.ptr)) {
		fprintf(stderr, "error: file "sfmt" not found\n", sarg(file_path));
		exit(1);
	}

	lexer lex = lexer_from_file(file_path);
	program prg = parse_program(&lex);
	intermediate_representation ir = compile(prg);
	if (error_count > 0) {
		fprintf(stderr, "error: %d errors occured during compilation\n", error_count);
		exit(1);
	}

	if (command == CMD_RUN) {
		simulate(ir);
	} else {
		codegen(ir, file_path, detect_host_machine());
	}

	return 0;
}