#include "codegen/arm64.c"

void codegen(intermediate_representation ir, string file_path, machine target) {
	string exe_path = string_split_last(file_path, '.').first;
	string asm_path = tconcat(exe_path, sv(".asm"));

	if (target.system == system_macos && target.arch == arch_arm64) {
		codegen_for_arm64_macos(ir, asm_path);
		exegen_for_arm64_macos(exe_path, asm_path);
	} else {
		todo("codegen for target");
	}
}
