bool inside_function = false;

#include "codegen/arm64_macos.c"
#include "codegen/x64_linux.c"

void codegen(intermediate_representation ir, string file_path, machine target) {
	fprintf(stderr, "info: target: "sfmt"\n", sarg(machine_to_string(target)));

	if (target.system == system_macos && target.arch == arch_arm64) {
		string exe_path = string_split_last(file_path, '.').first;
		string asm_path = tconcat(exe_path, sv(".asm"));

		codegen_for_arm64_macos(ir, asm_path);
		exegen_for_arm64_macos(exe_path, asm_path);
	} else if (target.system == system_linux && target.arch == arch_x64) {
		string exe_path = string_split_last(file_path, '.').first;
		string asm_path = tconcat(exe_path, sv(".S"));

		codegen_for_x64_linux(ir, asm_path);
		exegen_for_x64_linux(exe_path, asm_path);
	} else {
		todo("codegen for target");
	}
}
