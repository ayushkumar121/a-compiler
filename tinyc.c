int error_count;

#include "basic.c"
#include "arena.c"
#include "lexer.c"
#include "types.c"
#include "parser.c"
#include "compiler.c"
#include "simulator.c"
#include "codegen.c"

struct {
	bool build;
} flags;

int main(int argc, char** argv) {
	arena_init(&ga, GLOBAL_ARENA_CAP);
	Args args = {1, argc, argv};

	string file_path = args_next(&args);
	if (file_path.len == 0) {
		fprintf(stderr, "error: no file provided\n");
		exit(1);
	}

	string flag = flag = args_next(&args);
	while(flag.len != 0) {
		if (string_eq(flag, sv("--build")) || string_eq(flag, sv("-b"))) {
			flags.build = true;
		} else {
			fprintf(stderr, "error: unknown command "sfmt"\n", sarg(flag));
			exit(1);
		}
		flag = args_next(&args);
	}

	if (!file_exists(file_path.ptr)) {
		fprintf(stderr, "error: file "sfmt" not found\n", sarg(file_path));
		exit(1);
	}

	machine target = detect_host_machine();
	lexer lex = lexer_from_file(file_path);
	program prg = parse_program(&lex);
	intermediate_representation ir = compile(prg);
	if (error_count > 0) {
		fprintf(stderr, "error: %d errors occured during compilation\n", error_count);
		exit(1);
	}

	if (flags.build) {
		codegen(ir, file_path, target);
	}

	// simulation is the default mode
	simulate(ir);

	return 0;
}
