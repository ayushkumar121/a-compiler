int error_count;

#include "basic.c"
#include "lexer.c"
#include "types.c"
#include "parser.c"
#include "compiler.c"
#include "simulator.c"
#include "codegen.c"

struct {
	bool build;
	strings libs;
} flags;

void print_usage(void) {
	printf("USAGE: tinyc [options] file\n");
	printf("OPTIONS: \n");
	printf("\t --build/-b: build program into executable\n");
	printf("\t --lib/-l: provide library (.so or .dylib)\n");
	printf("\t --help/-h: print this menu\n");
	printf("\n");
}

int main(int argc, char** argv) {
	Args args = {1, argc, argv};

	string file_path;
	int i = 0;
	string arg = args_next(&args);
	while(arg.len != 0) {
		if (arg.ptr[0] == '-') {
			// Parsing flags
			if (string_eq(arg, sv("--build")) || string_eq(arg, sv("-b"))) {
				flags.build = true;
			} else if (string_eq(arg, sv("--lib")) || string_eq(arg, sv("-l"))) {
				array_append(&flags.libs, args_next(&args));
			} else if (string_eq(arg, sv("--help")) || string_eq(arg, sv("-h"))) {
				print_usage();
				exit(0);
			} else {
				fprintf(stderr, "error: unknown flag "sfmt"\n", sarg(arg));
				exit(1);
			}
		} else {
			// Parsing positional arguments
			switch(i) {
			case 0: 
				file_path = arg;
				break;

			default: 
				fprintf(stderr, "error: unknown positional argument "sfmt"\n", sarg(arg));
				exit(1);
			}
			i++;
		}
		arg = args_next(&args);
	}

	
	if (file_path.len == 0) {
		fprintf(stderr, "error: no file provided\n");
		exit(1);
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
