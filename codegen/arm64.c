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

void load_arg(FILE* out, int reg, argument src, int offset) {
	char reg_size = src.size <= 4 ? 'w' : 'x';

	switch(src.type) {
	case argument_literal:
		ASSERT(src.size <= 8);
		load_immediate(out, reg, src.size, src.value);
		break;

	case argument_local:
		fprintf(out, "   ldr %c%d, [x29, #-%zu]\n", reg_size, reg, 16+src.offset+offset);
		break;

	case argument_global:
	    fprintf(out, "   adrp x9, _globals@PAGE\n");
	    fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
	    fprintf(out, "   ldr %c%d, [x9, #%zu]\n", reg_size, reg, src.offset+offset);
		break;

	default: unreachable;
	}
}

void store_arg(FILE* out, int reg, argument dst, int offset) {
	char reg_size = dst.size <= 4? 'w':'x';
	switch(dst.type) {
	case argument_local:
		fprintf(out, "   str %c%d, [x29, #-%zu]\n", reg_size, reg, 16+dst.offset+offset);
		break;

	case argument_global:
    	fprintf(out, "   adrp x9, _globals@PAGE\n");
		fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
		fprintf(out, "   str %c%d, [x9, #%zu]\n", reg_size, reg, dst.offset+offset);
		break;
		
	default: unreachable;
	}
}
void codegen_for_arm64_macos(intermediate_representation ir, string asm_path) {
	fprintf(stderr, "info: generating %.*s\n", string_arg(asm_path));
	FILE* out = fopen(asm_path.ptr, "w+");

	fprintf(out, ".text\n");
	fprintf(out, ".global _start\n\n");
	fprintf(out, "_start:\n");
	fprintf(out, "   bl _main\n");
	fprintf(out, "   movz x16, #0x1\n");
	fprintf(out, "   movk x16, #0x200, lsl #16\n");
	fprintf(out, "   svc #0\n");

	string current_func_name;
	size_t frame_size;
	for (int i=0; i<ir.instructions.len; i++) {
		instruction in = ir.instructions.ptr[i];
		switch(in.type){
		case INS_FUNC_START: {
			current_func_name = in.label;

			fprintf(out, "; INS_FUNC_START\n");
			fprintf(out, "_%.*s:\n", string_arg(current_func_name));
			fprintf(out, "   stp x29, x30, [sp, #-16]!\n");
			fprintf(out, "   mov x29, sp\n");
			assert(in.as.func.frame != NULL);
			frame_size = align(in.as.func.frame->size, 16);
			fprintf(out, "   sub sp, sp, #%zu\n", frame_size);
		} break;
		case INS_RET: {
			fprintf(out, "; INS_RET\n");
			load_arg(out, 0, in.as.op.dst, 0);
			fprintf(out, "   b .Lreturn_%.*s\n", string_arg(current_func_name));
		} break;
		case INS_FUNC_END: {
			fprintf(out, "; INS_FUNC_END\n");

			fprintf(out, ".Lreturn_%.*s:\n", string_arg(current_func_name));
			size_t stack_size = align(frame_size, 16);
			fprintf(out, "   add sp, sp, #%zu\n", stack_size);
			fprintf(out, "   ldp x29, x30, [sp], #16\n");
			fprintf(out, "   ret\n");
		} break;
		case INS_LEA: {
			fprintf(out, "; INS_LEA\n");

			switch(in.as.op.src1.type) {
			case argument_local:
				fprintf(out, "   sub x0, x29, #%zu\n", 16+in.as.op.src1.offset);
				break;

			case argument_global:
		    	fprintf(out, "   adrp x0, _globals@PAGE\n");
				fprintf(out, "   add x0, x0, _globals@PAGEOFF\n");
				fprintf(out, "   add x0, x0, #%zu\n", in.as.op.src1.offset);
				break;

			default: unreachable;
			}
			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		case INS_COPY: {
			fprintf(out, "; INS_COPY\n");
			ASSERT(in.as.op.src2.type == argument_literal);

			int size = in.as.op.src2.value;
			if (size <= 8) {
				load_arg(out, 0, in.as.op.src1, 0);
				load_arg(out, 1, in.as.op.dst, 0);

			    fprintf(out, "   ldr x2, [x0, #0]\n");
			    fprintf(out, "   str x2, [x1, #0]\n");
			} else if (size <= 16) {
				load_arg(out, 0, in.as.op.src1, 0);
				load_arg(out, 1, in.as.op.dst, 0);

			    fprintf(out, "   ldr x2, [x0, #0]\n");
			    fprintf(out, "   ldr x3, [x0, #8]\n"); 
			    fprintf(out, "   str x2, [x1, #0]\n");
			    fprintf(out, "   str x3, [x1, #8]\n");
			} else {
				load_arg(out, 0, in.as.op.src1, 0); // src = x0
				load_arg(out, 1, in.as.op.dst, 0); // dst = x1
				load_immediate(out, 2, 8, size); // bytes remaining
			    fprintf(out, "   mov x3, #0\n"); // offset = 0
			    fprintf(out, "1:\n");
			    fprintf(out, "   ldrb w6, [x0, x3]\n"); // load byte from src
			    fprintf(out, "   strb w6, [x1, x3]\n"); // store byte to dst
			    fprintf(out, "   add x3, x3, #1\n"); // offset++
			    fprintf(out, "   cmp x3, x2\n"); // check if done
			    fprintf(out, "   b.lt 1b\n");  // loop if offset < size
			}
		} break;
		case INS_LSTR: {
			fprintf(out, "; INS_LSTR\n");

			ASSERT(in.as.op.src1.type == argument_string);
			string str = ir.strings.ptr[in.as.op.src1.offset];

			load_immediate(out, 0, 8, str.len);
			store_arg(out, 0, in.as.op.dst, 0);

	    	fprintf(out, "   adrp x0, .LC%zu@PAGE\n", in.as.op.src1.offset);
			fprintf(out, "   add x0, x0, .LC%zu@PAGEOFF\n", in.as.op.src1.offset);

			store_arg(out, 0, in.as.op.dst, 8);
		} break;
		case INS_LPARAM: {
			fprintf(out, "; INS_LPARAM\n");

			ASSERT(in.as.op.src1.type == argument_param);
			int index = in.as.op.src1.offset;
			if (index <= 8) {
				if (in.as.op.src1.size <= 8 ) {
					store_arg(out, index, in.as.op.dst, 0);
				} else if (in.as.op.src1.size <= 16) {
					store_arg(out, index, in.as.op.dst, 0);
					store_arg(out, index+1, in.as.op.dst, 8);
				} else {
					todo("support bigger struct types");
				}
			} else {
				todo("support more than 8 arguments");
			}
		} break;
		case INS_CALL: {
			fprintf(out, "; INS_CALL\n");

			if (in.as.call.params.len > 8) todo("support more arguments than 8");
			int i = 0;
			while(i<min(in.as.call.params.len, 8)) {
				argument arg = in.as.call.params.ptr[i];
				if (arg.size <= 8 ) {
					load_arg(out, i++, arg, 0);
				} else if (arg.size <= 16) {
					load_arg(out, i++, arg, 0);
					load_arg(out, i++, arg, 8);
				} else {
					todo("support bigger struct types");
				}
			}
			fprintf(out, "   bl _%.*s\n", string_arg(in.label));
			assert(in.as.op.dst.size <= 8 && "support bigger return type");

			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		case INS_LOAD: {
			fprintf(out, "; INS_LOAD\n");
			ASSERT(in.as.op.src1.value_type == argument_value_ref);
			char reg_size = in.as.op.src1.size <= 4 ? 'w' : 'x';

			load_arg(out, 0, in.as.op.src1, 0);
			fprintf(out, "   ldr %c0, [x0]\n", reg_size);
			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		case INS_STORE: {
			fprintf(out, "; INS_STORE\n");
			char reg_size = in.as.op.src1.size <= 4 ? 'w' : 'x';

			load_arg(out, 0, in.as.op.src1, 0);
			
			if (in.as.op.dst.value_type == argument_value_ref) {
				load_arg(out, 1, in.as.op.dst, 0);
				fprintf(out, "   str %c0, [x1]\n", reg_size);
			} else {
				store_arg(out, 0, in.as.op.dst, 0);
			}
		} break;
		case INS_ADD: {
			fprintf(out, "; INS_ADD\n");

			load_arg(out, 0, in.as.op.src1, 0);
			load_arg(out, 1, in.as.op.src2, 0);
			fprintf(out, "   add x0, x0, x1\n");
			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		case INS_SUB: {
			fprintf(out, "; INS_SUB\n");

			load_arg(out, 0, in.as.op.src1, 0);
			load_arg(out, 1, in.as.op.src2, 0);
			fprintf(out, "   sub x0, x0, x1\n");
			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		case INS_MUL: {
			fprintf(out, "; INS_MUL\n");

			load_arg(out, 0, in.as.op.src1, 0);
			load_arg(out, 1, in.as.op.src2, 0);
			fprintf(out, "   mul x0, x0, x1\n");
			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		case INS_MADD: {
			fprintf(out, "; INS_MADD\n");

			load_arg(out, 0, in.as.op.src1, 0);
			load_arg(out, 1, in.as.op.src2, 0);
			load_arg(out, 2, in.as.op.src3, 0);
			fprintf(out, "   madd x0, x0, x1, x2\n");
			store_arg(out, 0, in.as.op.dst, 0);
		} break;

		case INS_DIV: {
			fprintf(out, "; INS_DIV\n");

			load_arg(out, 0, in.as.op.src1, 0);
			load_arg(out, 1, in.as.op.src2, 0);
			fprintf(out, "   sdiv x0, x0, x1\n");
			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		default: {
			printf("%d\n", in.type);
			unreachable;
		}
		}
	}

	fprintf(out, ".section __TEXT,__cstring,cstring_literals\n");
	for (int i=0; i<ir.strings.len; i++) {
		fprintf(out, ".LC%d:\n", i);
		fprintf(out, "  .string \"%.*s\"\n", string_arg(string_unescape(ir.strings.ptr[i])));
	}

	fclose(out);
}

void exegen_for_arm64_macos(string exe_path, string asm_path) {
	fprintf(stderr, "info: generating %.*s\n", string_arg(exe_path));

	cmd(tsprintf("as -o %.*s.o %.*s", 
		string_arg(exe_path), string_arg(asm_path)));

	cmd(tsprintf("ld -o %.*s %.*s.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _start", 
		string_arg(exe_path), string_arg(exe_path)));
}
