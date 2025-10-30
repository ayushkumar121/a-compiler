void load_immediate(FILE* out, int reg, size_t size, size_t value) {
	char reg_size = size <= 4 ? 'w' : 'x';

	if (value <= 0xFFFF) {
    	fprintf(out, "   movz %c%d, #%zu\n", reg_size, reg, value);
	} else {
    	fprintf(out, "   movz %c%d, #%zu\n", reg_size, reg, value & 0xFFFF);
    	if ((value >> 16) & 0xFFFF) {
        	fprintf(out, "   movk %c%d, #%zu, lsl #16\n", reg_size, reg, (value >> 16) & 0xFFFF);
    	}
    	if ((value >> 32) & 0xFFFF) {
        	fprintf(out, "   movk %c%d, #%zu, lsl #32\n", reg_size, reg, (value >> 32) & 0xFFFF);
    	}
    	if ((value >> 48) & 0xFFFF) {
        	fprintf(out, "   movk %c%d, #%zu, lsl #48\n", reg_size, reg, (value >> 48) & 0xFFFF);
    	}
	}
}

void load_arg(FILE* out, int reg, argument arg) {
	switch(arg.type) {
	case argument_type_none: break;

	case argument_type_literal: {
    	switch(arg.as.literal.type) {
    	case argument_literal_type_integer: {
	    	load_immediate(out, reg, arg.size, arg.as.literal.as.integer.value);
    	} break;
    	case argument_literal_type_string: {
    		fprintf(out, "   adrp x9, .S%zu@PAGE\n", arg.as.literal.as.string.offset);
	    	fprintf(out, "   add x%d, x9, .S%zu@PAGEOFF\n", reg, arg.as.literal.as.string.offset);
    	} break;
    	}
	} break;

	case argument_type_local_var: {
		char reg_size = arg.size <= 4 ? 'w' : 'x';
		fprintf(out, "   ldr %c%d, [x29, #-%zu]\n", reg_size, reg, 16+arg.as.var.offset);
	} break;

	case argument_type_global_var: {
	    char reg_size = arg.size <= 4 ? 'w' : 'x';
	    fprintf(out, "   adrp x9, _globals@PAGE\n");
	    fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
	    fprintf(out, "   ldr %c%d, [x9, #%zu]\n", reg_size, reg, arg.as.var.offset);
	} break;	
	}
}

void codegen_for_arm64_macos(intermediate_representation ir, string asm_path) {
	print(sv("info: generating ")); println(asm_path);

	FILE* out = fopen(asm_path.ptr, "w+");

	fprintf(out, ".text\n");
	fprintf(out, ".global _start\n\n");
	fprintf(out, "_start:\n");
	fprintf(out, "   bl main\n");
	fprintf(out, "   movz x16, #0x1\n");
	fprintf(out, "   movk x16, #0x200, lsl #16\n");
	fprintf(out, "   svc #0\n");

	fprintf(out, "write:\n");
	fprintf(out, "   movz x16, #04\n");
	fprintf(out, "   movk x16, #0x200, lsl #16\n");
	fprintf(out, "   svc #0\n");
	fprintf(out, "   ret\n");

	fprintf(out, "len:\n");
	fprintf(out, "   ldr x0, [x0, #8]\n");
	fprintf(out, "   ret\n");
	fprintf(out, "\n");

	string current_func_name = sv("");
	for (int i=0; i<ir.instructions.len; i++) {
		instruction in = ir.instructions.ptr[i];
		switch(in.type){
			case INS_FUNC_START: {
				fprintf(out, "; INS_FUNC_START\n");

				current_func_name = in.as.func.identifier;
				fprintf(out, "%.*s:\n", string_arg(current_func_name));
				fprintf(out, "   stp x29, x30, [sp, #-16]!\n");
				fprintf(out, "   mov x29, sp\n");
				assert(in.as.func.stack_size != NULL);
				size_t stack_size = align(*in.as.func.stack_size, 16);
				fprintf(out, "   sub sp, sp, #%zu\n", stack_size);
			}	break;

			case INS_FUNC_END: {
				fprintf(out, "; INS_FUNC_END\n");

				fprintf(out, ".Lreturn_%.*s:\n", string_arg(current_func_name));
				size_t stack_size = align(*in.as.func.stack_size, 16);
				assert(in.as.func.stack_size != NULL);
			    fprintf(out, "   add sp, sp, #%zu\n", stack_size);
				fprintf(out, "   ldp x29, x30, [sp], #16\n");
				fprintf(out, "   ret\n");
			} break;

			case INS_ASSIGN: {
				fprintf(out, "; INS_ASSIGN\n");

				load_arg(out, 0, in.as.assign.value);

				if (in.as.assign.destination.type == argument_type_local_var) {
					fprintf(out, "   str x0, [x29, #-%zu]\n", 16+in.as.assign.destination.as.var.offset);
				} else if (in.as.assign.destination.type == argument_type_global_var) {
				    fprintf(out, "   adrp x9, _globals@PAGE\n");
	    			fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
					fprintf(out, "   str x0, [x9, #%zu]\n", in.as.assign.destination.as.var.offset);
				} else unreachable;


				if(in.as.assign.value.type == argument_type_literal &&
					in.as.assign.value.as.literal.type == argument_literal_type_string) {
	    			load_immediate(out, 0, 8, in.as.assign.value.as.literal.as.string.len);

					size_t len_offset = 16+in.as.assign.destination.as.var.offset + 8;
					if (in.as.assign.destination.type == argument_type_local_var) {
						fprintf(out, "   str w0, [x29, #-%zu]\n", len_offset);
					} else if (in.as.assign.destination.type == argument_type_global_var) {
					    fprintf(out, "   adrp x9, _globals@PAGE\n");
		    			fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
						fprintf(out, "   str w0, [x9, #%zu]\n", len_offset);
					} else unreachable;

				}

			} break;

			case INS_INTRINSIC: {
				fprintf(out, "; INS_INTRINSIC\n");
				char reg_size = in.as.intrinsic.destination.size <= 4 ? 'w' : 'x';
				switch(in.as.intrinsic.type) {

				case intrinsic_add:
					load_arg(out, 0, in.as.intrinsic.args[0]);
					load_arg(out, 1, in.as.intrinsic.args[1]);
					fprintf(out, "   add %c0, %c0, %c1\n", reg_size, reg_size, reg_size);
					break;
				case intrinsic_sub:
					load_arg(out, 0, in.as.intrinsic.args[0]);
					load_arg(out, 1, in.as.intrinsic.args[1]);
					fprintf(out, "   sub %1$c0, %1$c0, %1$c1\n", reg_size);
					break;
				case intrinsic_mul:
					load_arg(out, 0, in.as.intrinsic.args[0]);
					load_arg(out, 1, in.as.intrinsic.args[1]);
					fprintf(out, "   mul %1$c0, %1$c0, %1$c1\n", reg_size);
					break;
				case intrinsic_div: 
					load_arg(out, 0, in.as.intrinsic.args[0]);
					load_arg(out, 1, in.as.intrinsic.args[1]);
					fprintf(out, "   sdiv %1$c0, %1$c0, %1$c1\n", reg_size);
					break;

				case intrinsic_ref: {
					argument arg = in.as.intrinsic.args[0];
					if (arg.type == argument_type_local_var) {
						fprintf(out, "  add x0, sp, #%zu\n", arg.as.var.offset);
					} else if (arg.type == argument_type_global_var) {
				    	fprintf(out, "   adrp x9, _globals@PAGE\n");
	    				fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
        				fprintf(out, "   add x0, x9, #%zu\n", arg.as.var.offset);
					}
				} break;

			    case intrinsic_index: {
					argument arg = in.as.intrinsic.args[0];
					size_t index = in.as.intrinsic.args[1].as.literal.as.integer.value;
					if (arg.type == argument_type_local_var) {
						fprintf(out, "  ldr %c0, [sp, #%zu]\n", reg_size, arg.as.var.offset + arg.stride * index);
					} else if (arg.type == argument_type_global_var) {
				    	fprintf(out, "   adrp x9, _globals@PAGE\n");
	    				fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
        				fprintf(out, "   ldr %c0, [x9, #%zu]\n", reg_size, arg.as.var.offset + arg.stride * index);
					}
				} break;
				}
				fprintf(out, "   str %c0, [x29, #-%zu]\n", reg_size, 16+in.as.intrinsic.destination.as.var.offset);
			} break;

			case INS_RETURN: {
				fprintf(out, "; INS_RETURN\n");
			    load_arg(out, 0, in.as.ret.value);
			    fprintf(out, "   b .Lreturn_%.*s\n", string_arg(current_func_name));
			} break;

			case INS_CALL: {
				fprintf(out, "; INS_CALL\n");

				if (in.as.func_call.argc > 8) {
					todo("support func call with more than 8 args");
				}
				for (int i=0; i<in.as.func_call.argc; i++) {
					load_arg(out, i, in.as.func_call.args[i]);
				}

				fprintf(out, "   bl %.*s\n", string_arg(in.as.func_call.identifier));

				if (in.as.func_call.destination.type == argument_type_local_var) {
					fprintf(out, "   str x0, [x29, #-%zu]\n", 16+in.as.func_call.destination.as.var.offset);
				} else if (in.as.func_call.destination.type == argument_type_global_var) {
				    fprintf(out, "   adrp x9, _globals@PAGE\n");
	    			fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
					fprintf(out, "   str x0, [x9, #%zu]\n", in.as.func_call.destination.as.var.offset);
				}
			} break;

			case INS_LABEL: {
				fprintf(out, "; INS_LABEL\n");
				fprintf(out, "%d:\n", in.as.label.index);
			} break;

			case INS_IF: {
				fprintf(out, "; INS_IF\n");
				load_arg(out, 0, in.as.jmp.condition);
				fprintf(out, "   cmp w0, #0\n");
				fprintf(out, "   b.eq %df\n", in.as.jmp.label);
				fprintf(out, "   b.lt %df\n", in.as.jmp.label);
			} break;

			case INS_WHILE: {
				fprintf(out, "; INS_WHILE\n");
				load_arg(out, 0, in.as.jmp.condition);
				fprintf(out, "   cmp w0, #0\n");
				fprintf(out, "   b.eq %df\n", in.as.jmp.label);
				fprintf(out, "   b.lt %df\n", in.as.jmp.label);
			} break;

			case INS_END_WHILE: {
				fprintf(out, "; INS_END_WHILE\n");
				fprintf(out, "   b %db\n", in.as.jmp.label);
			} break;

			default: unreachable
		}
	}

	fprintf(out, "\n.data\n");
	fprintf(out, "_globals:\n");
	for (int i=0; i<ir.globals.len; i++) {
		global glb = ir.globals.ptr[i];
		fprintf(out, "%.*s:\n", string_arg(glb.identifier));
		
		if (glb.value.type == global_value_type_number) {
			if (glb.size == 1) {
				fprintf(out, ".align 0\n");
				fprintf(out, "  .byte %zu\n", glb.value.as.number);
			} else {
				fprintf(out, ".balign %zu\n", glb.size);
				fprintf(out, "  .%zubyte %zu\n", glb.size, glb.value.as.number);
			}
		} else if (glb.value.type == global_value_type_string) {
			fprintf(out, ".align 3\n");
			fprintf(out, "  .xword .S%zu\n", glb.value.as.string.offset);
			fprintf(out, "  .xword %zu\n", glb.value.as.string.size);
		} else if (glb.value.type == global_value_type_array) {
			fprintf(out, "  .byte ");
			for (int i=0; i<glb.value.as.array.len; i++) {
				fprintf(out, "  %x", glb.value.as.array.data[i]);
			}
			fprintf(out, "\n");
		} else {
			fprintf(out, "  .space %zu\n", glb.size);
		}
	}

	fprintf(out, "\n.align 0\n");
	fprintf(out, "_strings:\n");
	for (int i=0; i<ir.strings.len; i++) {
		fprintf(out, ".S%d:\n", i);
		fprintf(out, "  .string \"%.*s\"\n", string_arg(string_unescape(ir.strings.ptr[i])));
	}

	fclose(out);
}

void exegen_for_arm64_macos(string exe_path, string asm_path) {
	print(sv("info: generating ")); println(exe_path);

	cmd(tsprintf("as -o %.*s.o %.*s", 
		string_arg(exe_path), string_arg(asm_path)));

	cmd(tsprintf("ld -o %.*s %.*s.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _start", 
		string_arg(exe_path), string_arg(exe_path)));
}

