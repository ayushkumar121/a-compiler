typedef enum {
	RDI, RSI, RDX, RCX, R8, R9,
	R10, R11, R12, R13, R14, R15, RAX, RBX, RSP, RBP, x64_register_count
} x64_register;

// x64_register x64_vreg_maping[vreg_count] = {
// 	[VR0] = R10,
// 	[VR1] = R11,
// 	[VR2] = R12,
// 	[VR3] = R13,
// };

const char* x64_register_name(x64_register reg, int size)
{
    int idx =
        (size == 1) ? 0 :
        (size == 4) ? 1 :
        (size == 8) ? 2 :
        -1;
	if (idx < 0) unreachable();
	static const char* names[x64_register_count][3] = {
	    [RDI] = {"dil",  "edi",  "rdi"},
	    [RSI] = {"sil",  "esi",  "rsi"},
	    [RDX] = {"dl",   "edx",  "rdx"},
	    [RCX] = {"cl",   "ecx",  "rcx"},
	    [R8]  = {"r8b",  "r8d",  "r8"},
	    [R9]  = {"r9b",  "r9d",  "r9"},
	    [R10] = {"r10b", "r10d", "r10"},
	    [R11] = {"r11b", "r11d", "r11"},
	    [R12] = {"r12b", "r12d", "r12"},
	    [R13] = {"r13b", "r13d", "r13"},
	    [R14] = {"r14b", "r14d", "r14"},
	    [R15] = {"r15b", "r15d", "r15"},
	    [RAX] = {"al",   "eax",  "rax"},
	    [RBX] = {"bl",   "ebx",  "rbx"},
	    [RSP] = {"sp",   "esp",  "rsp"},
	    [RBP] = {"bp",   "ebp",  "rbp"},
	};

    return names[reg][idx];
}

char register_size(int size) {
	switch(size) {
	case 1: return 'b';
	case 2: return 'w';
	case 4: return 'l';
	case 8: return 'q';
	}
	unreachable();
}

void x64_load_addr(FILE* out, x64_register reg, argument src) {
	switch(src.type) {
	case argument_type_local:
		fprintf(out, "  leaq -%d(%%rbp), %%%s\n", src.as.offset, x64_register_name(reg, src.size));
		break;

	default: unreachable();
	}
}

void x64_load(FILE* out, x64_register reg, argument src) {
	ASSERT(src.size > 0);
	ASSERT(src.size <= 8);
	char reg_size = register_size(src.size);

	switch(src.type) {
	case argument_type_literal:
		fprintf(out, "  mov%c $%d, %%%s\n", reg_size, src.as.value, x64_register_name(reg, src.size));
		break;

	case argument_type_local:
		fprintf(out, "  mov%c -%d(%%rbp), %%%s\n", reg_size, src.as.offset, x64_register_name(reg, src.size));
		break;

	case argument_type_string:
		fprintf(out, "  movq $.LC%d, %%%s\n", src.as.index, x64_register_name(reg, src.size));
		break;

	default: unreachable();
	}
}

void x64_store(FILE* out, x64_register reg, argument dst) {
	if(dst.size == 0) return;

	ASSERT(dst.size <= 8);
	char reg_size = register_size(dst.size);

	switch(dst.type) {
	case argument_type_local:
		fprintf(out, "  mov%c %%%s, -%d(%%rbp)\n", reg_size, x64_register_name(reg, dst.size), dst.as.offset);
		break;

	default: unreachable();
	}
}

void x64_store_indirect(FILE* out, x64_register src, argument dst, int offset) {
	if(dst.size == 0) return;

	ASSERT(dst.size <= 8);
	char reg_size = register_size(dst.size);

	switch(dst.type) {
	case argument_type_local:
		fprintf(out, "  mov%c %d(%%%s), %%rax\n", reg_size, offset, x64_register_name(src, dst.size));
		fprintf(out, "  mov%c %%rax, -%d(%%rbp)\n", reg_size, dst.as.offset);
		break;

	default: unreachable();
	}
}

void x64_push(FILE* out, argument src) {
	ASSERT(src.size <= 8);
	fprintf(out, "  subq $8, %%rsp\n");
	char reg_size = register_size(src.size);
	switch(src.type) {
	case argument_type_literal:
		fprintf(out, "  mov%c $%d, (%%rsp)\n", reg_size, src.as.value);
		break;

	case argument_type_local:
		fprintf(out, "  mov%c -%d(%%rbp), %%rax\n", reg_size, src.as.offset);
		fprintf(out, "  mov%c %%rax, (%%rsp)\n", reg_size);
		break;

	default: unreachable();
	}
}

void x64_memcpy(FILE* out, x64_register dst, x64_register src, int n) {
	ASSERT(n >= 0);
	if (n == 0) return;
	fprintf(out, "  movq $%d, %%rcx\n", n);
	fprintf(out, "  movq %%%s, %%rsi\n", x64_register_name(src, 8));
	fprintf(out, "  movq %%%s, %%rdi\n", x64_register_name(dst, 8));
	fprintf(out, "  rep movsb\n");
}

string x64_linux_label(string label) {
	if (!inside_function) {
		return label;
	} else {
		return tconcat(sv("."), label);
	}
}

void codegen_for_x64_linux(intermediate_representation ir, string asm_path) {
	fprintf(stderr, "info: generating %.*s\n", sarg(asm_path));
	FILE* out = fopen(asm_path.ptr, "w+");

	fprintf(out, ".section .text\n");
	fprintf(out, ".globl _start\n\n");
	fprintf(out, "_start:\n");
	fprintf(out, "  call main\n");
	fprintf(out, "  movq %%rax, %%rdi\n");
	fprintf(out, "  jmp exit\n\n");

	for (int i = 0; i <builtin_count; ++i){
		switch(i) {
		case builtin_print:
			fprintf(out, "print:\n");
			fprintf(out, "  movq %%rdi, %%rdx\n"); // sarg2 = arg0 (len)
			fprintf(out, "  movq $1, %%rdi\n"); // sarg0 = 1 (SYS_OUT)
			fprintf(out, "  movq $1, %%rax\n");
			fprintf(out, "  syscall\n");
			fprintf(out, "  ret\n\n");
			break;

		case builtin_exit:
			fprintf(out, "exit:\n");
			fprintf(out, "  movq $60, %%rax\n");
			fprintf(out, "  syscall\n");
		 	break;
		case builtin_count: break;
		}
	}
	fprintf(out, "\n");

	for (int i=0; i<ir.instructions.len; i++) {
		instruction ins = ir.instructions.ptr[i];
		switch(ins.type){
		case ins_label:
			fprintf(out, "# ins_label\n");
			fprintf(out, sfmt":\n", sarg(x64_linux_label(ins.as.label)));
			break;

		case ins_func_start:
			fprintf(out, "# ins_func_start\n");
			fprintf(out, "  pushq %%rbp\n");
			fprintf(out, "  movq %%rsp, %%rbp\n");
			fprintf(out, "  subq $%d, %%rsp\n", ins.as.frame->size);
			inside_function = true;
			break;

		case ins_func_end:
			fprintf(out, "# ins_func_end\n");
			fprintf(out, "  movq %%rbp, %%rsp\n");
			fprintf(out, "  pop %%rbp\n");
			fprintf(out, "  ret\n\n");
			inside_function = false;
			break;

		case ins_func_call: {
			fprintf(out, "# ins_func_call\n");
			// Passed via register
			int slot_index = 0;
			int i = 0;
			while (i < ins.as.fcall.argc && slot_index < 6) {
				argument arg = ins.as.fcall.args[i++];
				if (arg.size <= 8) {
					x64_load(out, (x64_register)slot_index++, arg);
				} else if (arg.size <= 16) {
					// TODO: find actual field size
					x64_load(out, (x64_register)slot_index++, argument_field(arg, 0, 8));
					x64_load(out, (x64_register)slot_index++, argument_field(arg, 8, 8));
				} else {
					x64_load_addr(out, (x64_register)slot_index++, arg);
				}
			}

			// Passing via stack
			int stack_size = 0;
			for (int k = i; k < ins.as.fcall.argc; k++) {
			    argument arg = ins.as.fcall.args[k];
			    if (arg.size <= 8)
			        stack_size += 8;
			    else if (arg.size <= 16)
			        stack_size += 16;
			    else
			        stack_size += 8;
			}

			stack_size = (stack_size + 15) & ~15;
			if (stack_size > 0) {
			    fprintf(out, "  subq $%d, %%rsp\n", stack_size);
			}

			int offset = 0;
			int j = ins.as.fcall.argc - 1;
			while (j >= i) {
			    argument arg = ins.as.fcall.args[j--];
			    if (arg.size <= 8) {
			        x64_load(out, RAX, arg);
			        fprintf(out, "  movq %%rax, %d(%%rsp)\n", offset);
			        offset += 8;
			    } else if (arg.size <= 16) {
			        // TODO: find actual field size
			        x64_load(out, RAX, argument_field(arg, 0, 8));
			        x64_load(out, RBX, argument_field(arg, 8, 8));
			        fprintf(out, "  movq %%rax, %d(%%rsp)\n", offset);
			        fprintf(out, "  movq %%rbx, %d(%%rsp)\n", offset + 8);
			        offset += 16;
			    } else {
			        x64_load_addr(out, RAX, arg);
			        fprintf(out, "  movq %%rax, %d(%%rsp)\n", offset);
			        offset += 8;
			    }
			}

			fprintf(out, "  call "sfmt"\n", sarg(ins.as.fcall.identifier));
			if (stack_size>0) fprintf(out, "  addq $%d, %%rsp\n", stack_size);
			x64_store(out, RAX, ins.as.fcall.dst);
		} break;

		case ins_func_load_params: {
			fprintf(out, "# ins_func_load_params\n");
			// Passing via registers
			int slot_index = 0;
			int i = 0;
			while (i < ins.as.params.argc && slot_index < 6) {
				argument dst = ins.as.params.args[i++];
				if (dst.size <= 8) {
					x64_store(out, (x64_register)slot_index++, dst);
				} else if (dst.size <= 16) {
					// TODO: find actual field size
					x64_store(out, (x64_register)slot_index++, argument_field(dst, 0, 8));
					x64_store(out, (x64_register)slot_index++, argument_field(dst, 8, 8));
				} else {
					x64_load_addr(out, RAX, dst);
					x64_memcpy(out, RAX, (x64_register)slot_index++, dst.size);
				}
			}

			// Passing via stack
			int stack_offset = 16;
			while(i < ins.as.params.argc) {
				argument dst = ins.as.params.args[i++];
				if (dst.size <= 8) {
					x64_store_indirect(out, RBP, dst, stack_offset);
					stack_offset += 8;
				} else if (dst.size <= 16) {
					// TODO: find actual field size
		            x64_store_indirect(out, RBP, argument_field(dst, 0, 8), stack_offset);
		            x64_store_indirect(out, RBP, argument_field(dst, 8, 8), stack_offset + 8);
					stack_offset += 16;
				} else {
		            fprintf(out, "  movq %d(%%rbp), %%rax\n", stack_offset);
		            x64_load_addr(out, RBX, dst);
		            x64_memcpy(out, RBX, RAX, dst.size);
		            stack_offset += 8;
				}
			}
		} break;

		case ins_ret:
			fprintf(out, "# ins_ret\n");
			if (ins.as.ret.type != argument_type_none) {
				x64_load(out, RAX, ins.as.ret);
			}
			break;

		case ins_binop: {
			switch(ins.as.op.type) {
			case op_store:
				fprintf(out, "# op_store\n");
				x64_load(out, RAX, ins.as.op.src1);
				x64_store(out, RAX, ins.as.op.dst);
				break;

			case op_load_indirect:
				fprintf(out, "# op_load_indirect\n");
				ASSERT(ins.as.op.dst.size <= 8);
				ASSERT(ins.as.op.src2.type != argument_type_none);
				x64_load(out, RAX, ins.as.op.src1);
				break;
			default: break;
			}
		} break;

		default: unreachable();
		}
	}

	if (ir.string_literals.len > 0) {
		fprintf(out, ".section .rodata\n");
		for (int i=0; i<ir.string_literals.len; i++) {
			fprintf(out, ".LC%d:\n", i);
			fprintf(out, "  .string \""sfmt"\"\n", sarg(string_unescape(ir.string_literals.ptr[i])));
		}
	}

	fclose(out);
}


void exegen_for_x64_linux(string exe_path, string asm_path) {
	fprintf(stderr, "info: generating "sfmt"\n", sarg(exe_path));

	cmd(tsprintf("as -o %.*s.o %.*s",
		sarg(exe_path), sarg(asm_path)));

	cmd(tsprintf("ld -o %.*s %.*s.o -e _start",
		sarg(exe_path), sarg(exe_path)));
}
