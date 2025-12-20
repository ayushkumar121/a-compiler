typedef enum {
    X0 = 0, X1, X2, X3, X4, X5, X6, X7,
    X8, X9, X10, X11, X12, X13, X14, X15,
    X16, X17, X18,
    X19, X20, X21, X22, X23, X24, X25, X26, X27, X28,
    X29,        // fp
    X30,        // lr
    SP,         // stack pointer
    arm64_register_count
} arm64_register;

arm64_register arm64_vreg_mapping[vreg_count] = {
	[VR0] = X9,
	[VR1] = X10,
	[VR2] = X11,
	[VR3] = X12,
};

const char* arm64_register_name(arm64_register reg, int size) {
    static const char* xnames[] = {
        "x0","x1","x2","x3","x4","x5","x6","x7",
        "x8","x9","x10","x11","x12","x13","x14","x15",
        "x16","x17","x18",
        "x19","x20","x21","x22","x23","x24","x25","x26","x27","x28",
        "x29","x30","sp"
    };

    static const char* wnames[] = {
        "w0","w1","w2","w3","w4","w5","w6","w7",
        "w8","w9","w10","w11","w12","w13","w14","w15",
        "w16","w17","w18",
        "w19","w20","w21","w22","w23","w24","w25","w26","w27","w28",
        "w29","w30","wsp"
    };

    if (size <= 4)
        return wnames[reg];
    if (size <= 8)
        return xnames[reg];

    unreachable();
}

void load_immediate(FILE* out, int reg, size_t size, size_t value) {
	char reg_size = size <= 4 ? 'w' : 'x';


	if (value <= 0xFFFF) {
    	fprintf(out, "  movz %c%d, #%zu\n", reg_size, reg, value);
	} else {
    	fprintf(out, "  movz %c%d, #%zu\n", reg_size, reg, value & 0xFFFF);
    	if ((value >> 16) & 0xFFFF) {
        	fprintf(out, "  movk %c%d, #%zu, lsl #16\n", reg_size, reg, (value >> 16) & 0xFFFF);
    	}
    	if ((value >> 32) & 0xFFFF) {
        	fprintf(out, "  movk %c%d, #%zu, lsl #32\n", reg_size, reg, (value >> 32) & 0xFFFF);
    	}
    	if ((value >> 48) & 0xFFFF) {
        	fprintf(out, "  movk %c%d, #%zu, lsl #48\n", reg_size, reg, (value >> 48) & 0xFFFF);
    	}
	}
}

void arm64_load(FILE* out, int reg, argument src) {
	ASSERT(src.size > 0);
	ASSERT(src.size <= PTR_SIZE);
	char reg_size = src.size <= 4 ? 'w' : 'x';

	switch(src.type) {
	case argument_type_literal:
		load_immediate(out, reg, src.size, src.as.value);
		break;

	case argument_type_vreg:
		ASSERT(reg != arm64_vreg_mapping[src.as.vreg]);
    	fprintf(out, "  mov %c%d, %c%d\n", reg_size, reg, reg_size, arm64_vreg_mapping[src.as.vreg]);
		break;

	case argument_type_local:
		if (src.as.offset >= -256 && src.as.offset <= 255) {
			fprintf(out, "  ldr %c%d, [x29, #-%d]\n", reg_size, reg, src.as.offset);
		} else {
			fprintf(out, "  sub x9, x29, #%d\n", src.as.offset);
			fprintf(out, "  ldr %c%d, [x9]\n", reg_size, reg);
		}
		break;

	case argument_type_global:
	    fprintf(out, "  adrp x9, _globals@PAGE\n");
	    fprintf(out, "  add x9, x9, _globals@PAGEOFF\n");
	    fprintf(out, "  ldr %c%d, [x9, #%d]\n", reg_size, reg, src.as.offset);
		break;

	case argument_type_string:
    	fprintf(out, "  adrp x%d, .LC%d@PAGE\n", reg, src.as.index);
		fprintf(out, "  add x%d, x%d, .LC%d@PAGEOFF\n", reg, reg, src.as.index);
		break;

	default: unreachable();
	}
}

void arm64_load_addr(FILE* out, int reg, argument src) {
	switch(src.type) {
	case argument_type_local:
		fprintf(out, "  sub x%d, x29, #%d\n", reg, src.as.offset);
		break;

	case argument_type_global:
		fprintf(out, "  adrp x0, _globals@PAGE\n");
		fprintf(out, "  add x0, x0, _globals@PAGEOFF\n");
		fprintf(out, "  add x%d, x0, #%d\n", reg, src.as.offset);
		break;

	default: unreachable();
	}
}

void arm64_store(FILE* out, int reg, argument dst) {
	if(dst.size == 0) return;
	ASSERT(dst.size <= PTR_SIZE);
	char reg_size = dst.size <= 4? 'w':'x';

	switch(dst.type) {
	case argument_type_vreg:
		ASSERT(reg != arm64_vreg_mapping[dst.as.vreg]);
    	fprintf(out, "  mov %c%d, %c%d\n", reg_size, arm64_vreg_mapping[dst.as.vreg], reg_size, reg);
		break;

	case argument_type_local:
		if (dst.as.offset >= -256 && dst.as.offset <= 255) {
			fprintf(out, "  str %c%d, [x29, #-%d]\n", reg_size, reg, dst.as.offset);
		} else {
			fprintf(out, "  sub x9, x29, #%d\n", dst.as.offset);
			fprintf(out, "  str %c%d, [x9]\n", reg_size, reg);
		}
		break;

	case argument_type_global:
    	fprintf(out, "  adrp x9, _globals@PAGE\n");
		fprintf(out, "  add x9, x9, _globals@PAGEOFF\n");
		fprintf(out, "  str %c%d, [x9, #%d]\n", reg_size, reg, dst.as.offset);
		break;

	default: unreachable();
	}
}

void arm64_store_indirect(FILE* out, arm64_register src, argument dst, int offset) {
	ASSERT(dst.size <= 8);
	if(dst.size == 0) return;
	char reg_size = dst.size <= 4? 'w':'x';

	switch(dst.type) {
	case argument_type_local:
		fprintf(out, "  ldr %c0, [%s, #%d]\n", reg_size, arm64_register_name(src, 8), offset);
		if (dst.as.offset >= -256 && dst.as.offset <= 255) {
			fprintf(out, "  str %c0, [x29, #-%d]\n", reg_size, dst.as.offset);
		} else {
			fprintf(out, "  sub x9, x29, #%d\n", dst.as.offset);
			fprintf(out, "  str %c0, [x9]\n", reg_size);
		}
		break;

	default: unreachable();
	}
}

void arm64_memcpy(FILE* out, arm64_register dst, arm64_register src, int n) {
	ASSERT(n >= 0);
	if (n == 0) return;
	fprintf(out, "  mov x9, #%d\n", n);
	fprintf(out, "1:\n");
	fprintf(out, "  cbz x9, 2f\n");
	fprintf(out, "  ldrb w10, [%s], #1\n", arm64_register_name(src, 8));
	fprintf(out, "  strb w10, [%s], #1\n",  arm64_register_name(dst, 8));
	fprintf(out, "  sub x9, x9, #1\n");
	fprintf(out, "  b 1b\n");
	fprintf(out, "2:\n");
}

string arm64_macos_label(string l) {
	if (!inside_function) {
		return tsprintf("_"sfmt, sarg(l));
	} else {
		return tsprintf("."sfmt, sarg(l));
	}
}

void codegen_for_arm64_macos(intermediate_representation ir, string asm_path) {
	fprintf(stderr, "info: generating %.*s\n", sarg(asm_path));
	FILE* out = fopen(asm_path.ptr, "w+");

	fprintf(out, ".text\n");
	fprintf(out, ".global _start\n\n");
	fprintf(out, "_start:\n");
	fprintf(out, "  bl _main\n");
	fprintf(out, "  b _exit\n");

	for (int i = 0; i <builtin_count; ++i){
		switch(i) {
		case builtin_print:
			fprintf(out, "_print:\n");
			fprintf(out, "  mov x2, x0\n");
			fprintf(out, "  mov x0, #1\n"); // SYS_OUT
			fprintf(out, "  movz x16, #0x4\n");
			fprintf(out, "  movk x16, #0x200, lsl #16\n");
			fprintf(out, "  svc #0\n");
			fprintf(out, "  ret\n");
			break;
		case builtin_exit:
			fprintf(out, "_exit:\n");
			fprintf(out, "  movz x16, #0x1\n");
			fprintf(out, "  movk x16, #0x200, lsl #16\n");
			fprintf(out, "  svc #0\n");
			fprintf(out, "  b .\n");
		 	break;
		case builtin_count: break;
		}
	}
	fprintf(out, "\n");

	size_t frame_size;
	for (int i=0; i<ir.instructions.len; i++) {
		instruction ins = ir.instructions.ptr[i];
		switch(ins.type){
		case ins_label:
			fprintf(out, "; ins_label\n");
			if (!inside_function) fprintf(out, ".global _"sfmt"\n", sarg(ins.as.label));
			fprintf(out, sfmt":\n", sarg(arm64_macos_label(ins.as.label)));
			break;

		case ins_func_start:
			fprintf(out, "; ins_func_start\n");
			fprintf(out, "  stp x29, x30, [sp, #-16]!\n");
			fprintf(out, "  mov x29, sp\n");
			frame_size = align(ins.as.frame->size, 16);
			fprintf(out, "  sub sp, sp, #%zu\n", frame_size);
			inside_function = true;
			break;

		case ins_func_end:
			fprintf(out, "; ins_func_end\n");
			fprintf(out, "  add sp, sp, #%zu\n", frame_size);
			fprintf(out, "  ldp x29, x30, [sp], #16\n");
			fprintf(out, "  ret\n\n");
			inside_function = false;
			break;

		case ins_func_call: {
			fprintf(out, "; ins_func_call\n");
			// Passed via register
			int slot_index = 0;
			int i = 0;
			while (i < ins.as.fcall.argc && slot_index < 8) {
				argument arg = ins.as.fcall.args[i++];
				if (arg.size <= 8) {
					arm64_load(out, (arm64_register)slot_index++, arg);
				} else if (arg.size <= 16) {
					// TODO: find actual field size
					arm64_load(out, (arm64_register)slot_index++, argument_field(arg, 0, 8));
					arm64_load(out, (arm64_register)slot_index++, argument_field(arg, 8, 8));
				} else {
					arm64_load_addr(out, (arm64_register)slot_index++, arg);
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
			stack_size = align(stack_size, 16);
			if (stack_size>0) fprintf(out, "  sub sp, sp, #%d\n", stack_size);

			int offset = 0;
			while (i < ins.as.fcall.argc) {
			    argument arg = ins.as.fcall.args[i++];
			    if (arg.size <= 8) {
			        arm64_load(out, X9, arg);
			        fprintf(out, "  str x9, [sp, #%d]\n", offset);
			        offset += 8;
			    } else if (arg.size <= 16) {
			        arm64_load(out, X9, argument_field(arg, 0, 8));
			        arm64_load(out, X10, argument_field(arg, 8, 8));
			        fprintf(out, "  str x9,  [sp, #%d]\n", offset);
			        fprintf(out, "  str x10, [sp, #%d]\n", offset + 8);
			        offset += 16;
			    } else {
			        arm64_load_addr(out, X9, arg);
			        fprintf(out, "  str x9, [sp, #%d]\n", offset);
			        offset += 8;
			    }
			}

			fprintf(out, "  bl _"sfmt"\n", sarg(ins.as.fcall.identifier));
			if (stack_size>0) fprintf(out, "  add sp, sp, #%d\n", stack_size);
			arm64_store(out, 0, ins.as.fcall.dst);
		} break;

		case ins_func_load_params: {
		    fprintf(out, "; ins_func_load_params\n");

		    int slot_index = 0;
		    int i = 0;

		    while (i < ins.as.params.argc && slot_index < 8) {
		        argument dst = ins.as.params.args[i++];
		        if (dst.size <= 8) {
		            arm64_store(out, (arm64_register)slot_index++, dst);
		        } else if (dst.size <= 16) {
		            arm64_store(out, (arm64_register)slot_index++, argument_field(dst, 0, 8));
		            arm64_store(out, (arm64_register)slot_index++, argument_field(dst, 8, 8));
		        } else {
		            // register holds pointer
		            arm64_load_addr(out, X1, dst);
		            arm64_memcpy(out, X1, (arm64_register)slot_index++, dst.size);
		        }
		    }

		    int stack_offset = 16;
		    while (i < ins.as.params.argc) {
		        argument dst = ins.as.params.args[i++];
		        if (dst.size <= 8) {
		            arm64_store_indirect(out, X29, dst, stack_offset);
		            stack_offset += 8;
		        } else if (dst.size <= 16) {
		            arm64_store_indirect(out, X29, argument_field(dst, 0, 8), stack_offset);
		            arm64_store_indirect(out, X29, argument_field(dst, 8, 8), stack_offset + 8);
		            stack_offset += 16;
		        } else {
		            fprintf(out, "  ldr x0, [x29, #%d]\n", stack_offset);
		            arm64_load_addr(out, X1, dst);
		            arm64_memcpy(out, X1, X0, dst.size);
		            stack_offset += 8;
		        }
		    }
		} break;

		case ins_ret:
			fprintf(out, "; ins_ret\n");
			if (ins.as.ret.type != argument_type_none) {
				arm64_load(out, 0, ins.as.ret);
			}
			break;

		case ins_binop: {
			switch(ins.as.op.type) {
			case op_add:
				fprintf(out, "; op_add\n");
				arm64_load(out, 0, ins.as.op.src1);
				arm64_load(out, 1, ins.as.op.src2);
				fprintf(out, "  add x0, x0, x1\n");
				arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_sub:
				fprintf(out, "; op_sub\n");
				arm64_load(out, 0, ins.as.op.src1);
				arm64_load(out, 1, ins.as.op.src2);
				fprintf(out, "  sub x0, x0, x1\n");
				arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_mul:
				fprintf(out, "; op_mul\n");
				arm64_load(out, 0, ins.as.op.src1);
				arm64_load(out, 1, ins.as.op.src2);
				fprintf(out, "  mul x0, x0, x1\n");
				arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_div:
				fprintf(out, "; op_div\n");
				arm64_load(out, 0, ins.as.op.src1);
				arm64_load(out, 1, ins.as.op.src2);
				fprintf(out, "  sdiv x0, x0, x1\n");
				arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_madd:
				fprintf(out, "; op_madd\n");
			    arm64_load(out, 0, ins.as.op.dst);
			    arm64_load(out, 1, ins.as.op.src1);
			    arm64_load(out, 2, ins.as.op.src2);
			    fprintf(out, "  madd x0, x1, x2, x0\n");
			    arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_lt:
				fprintf(out, "; op_lt\n");
				arm64_load(out, 0, ins.as.op.src1);
				arm64_load(out, 1, ins.as.op.src2);
				fprintf(out, "  cmp x0, x1\n");
				fprintf(out, "  cset x2, lt\n");
				arm64_store(out, 2, ins.as.op.dst);
				break;

			case op_gt:
				fprintf(out, "; op_gt\n");
				fprintf(out, "; op_lt\n");
				arm64_load(out, 0, ins.as.op.src1);
				arm64_load(out, 1, ins.as.op.src2);
				fprintf(out, "  cmp x0, x1\n");
				fprintf(out, "  cset x2, gt\n");
				arm64_store(out, 2, ins.as.op.dst);
				break;

			case op_addrof:
				fprintf(out, "; op_addrof\n");
				ASSERT(ins.as.op.dst.size == PTR_SIZE);
				arm64_load_addr(out, 0, ins.as.op.src1);
				arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_load_indirect:
				fprintf(out, "; op_load_indirect\n");
				ASSERT(ins.as.op.dst.size <= PTR_SIZE);
				ASSERT(ins.as.op.src2.type != argument_type_none);

				arm64_load(out, 0, ins.as.op.src1);
			    fprintf(out, "  ldr %c0, [x0]\n", ins.as.op.dst.size <= 4 ? 'w' : 'x');
				arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_store:
				fprintf(out, "; op_store\n");
				arm64_load(out, 0, ins.as.op.src1);
				arm64_store(out, 0, ins.as.op.dst);
				break;

			case op_store_indirect:
				fprintf(out, "; op_store_indirect\n");
				ASSERT(ins.as.op.dst.size <= PTR_SIZE);
				ASSERT(ins.as.op.src2.type != argument_type_none);

				arm64_load(out, 0, ins.as.op.src1);
    			arm64_load(out, 1, ins.as.op.dst);

				fprintf(out, "  str %c0, [x1]\n", ins.as.op.src1.size <= 4 ? 'w' : 'x');
				break;

			case op_copy:
			    fprintf(out, "; op_copy\n");
			    ASSERT(ins.as.op.src2.type == argument_type_literal);
			    int size = ins.as.op.src2.as.value;
			    arm64_load_addr(out, 0, ins.as.op.src1);  // src address -> x0
			    arm64_load_addr(out, 1, ins.as.op.dst);   // dst address -> x1
			    load_immediate(out, 2, 8, size);     // size -> x2
			    fprintf(out, "  mov x3, #0\n");     // offset = 0
			    fprintf(out, "1:\n");                // Loop label
			    fprintf(out, "  ldrb w4, [x0, x3]\n");  // Load byte from src
			    fprintf(out, "  strb w4, [x1, x3]\n");  // Store byte to dst
			    fprintf(out, "  add x3, x3, #1\n");     // offset++
			    fprintf(out, "  cmp x3, x2\n");         // Compare offset with size
			    fprintf(out, "  b.lt 1b\n");         // Loop if offset < size
			    break;

			default:{
				printf("unimplemented op %d\n", ins.as.op.type);
				unreachable();
			}
			}
		} break;

		case ins_jmp:
			fprintf(out, "  b "sfmt"\n", sarg(arm64_macos_label(ins.as.jmp.label)));
			break;

		case ins_jmp_ifnot:
			arm64_load(out, 0, ins.as.jmpifnot.cond);
			fprintf(out, "  cmp x0, #0\n");
			fprintf(out, "  beq "sfmt"\n", sarg(arm64_macos_label(ins.as.jmpifnot.label)));
			break;

		default: {
			printf("unimplemented ins %d\n", ins.type);
			unreachable();
		}
		}
	}

	if (ir.string_literals.len > 0) {
		fprintf(out, ".section __TEXT,__cstring,cstring_literals\n");
		for (int i=0; i<ir.string_literals.len; i++) {
			fprintf(out, ".LC%d:\n", i);
			fprintf(out, "  .string \""sfmt"\"\n", sarg(string_unescape(ir.string_literals.ptr[i])));
		}
	}

	fclose(out);
}

void exegen_for_arm64_macos(string exe_path, string asm_path) {
	fprintf(stderr, "info: generating "sfmt"\n", sarg(exe_path));

	cmd(tsprintf("as -o %.*s.o %.*s",
		sarg(exe_path), sarg(asm_path)));

	cmd(tsprintf("ld -o %.*s %.*s.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _start",
		sarg(exe_path), sarg(exe_path)));
}
