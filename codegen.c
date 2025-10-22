#include "codegen/arm64.c"

typedef enum {
	system_unknown,
	system_linux,
	system_windows,
	system_macos,
} operating_system;

typedef enum {
	arch_unknown,
	arch_x86,
	arch_x64,
	arch_arm64,
} architecture;

typedef struct {
	operating_system system;
	architecture arch;
} machine;

machine detect_host_machine(void) {
	machine m = {system_unknown, arch_unknown};

    #if defined(__APPLE__)
        m.system = system_macos;
    #elif defined(_WIN32) || defined(_WIN64)
        m.system = system_windows;
    #elif defined(__linux__)
        m.system = system_linux;
    #else
        m.system = system_unknown;
    #endif

    #if defined(__aarch64__) || defined(_M_ARM64)
        m.arch = arch_arm64;
    #elif defined(__x86_64__) || defined(_M_X64)
        m.arch = arch_x64;
    #elif defined(__i386__) || defined(_M_IX86)
        m.arch = arch_x86;
    #else
        m.arch = arch_unknown;
    #endif

	return m;
}

void codegen(instruction_list ins, string file_path, machine target) {
	string exe_path = string_split_last(file_path, '.').first;
	string asm_path = tconcat(exe_path, sv(".asm"));

	if (target.system == system_macos && target.arch == arch_arm64) {
		print(sv("info: generating ")); println(asm_path);
		codegen_for_arm64_macos(ins, asm_path);
	} else {
		todo("codegen for target");
	}
}
