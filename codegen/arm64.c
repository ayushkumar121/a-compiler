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

void load_arg(FILE* out, int reg, argument src) {
	char reg_size = src.size <= 4 ? 'w' : 'x';

	switch(src.type) {
	case argument_literal:
		if (src.value_type == argument_value_int) {
			load_immediate(out, reg, src.size, src.value);
		} else todo("implement argument_literal loading");
		break;

	case argument_local:
		fprintf(out, "   ldr %c%d, [x29, #-%zu]\n", reg_size, reg, 16+src.offset);
		break;

	case argument_global:
	    fprintf(out, "   adrp x9, _globals@PAGE\n");
	    fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
	    fprintf(out, "   ldr %c%d, [x9, #%zu]\n", reg_size, reg, src.offset);
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
		fprintf(out, "   add %c%d, x9, #%zu\n", reg_size, reg, dst.offset+offset);
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
			load_arg(out, 0, in.as.op.dst);
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

			load_arg(out, 0, in.as.op.dst); // src = x0
			load_arg(out, 1, in.as.op.src1); // dst = x1
			assert(in.as.op.src2.type == argument_literal);
			load_immediate(out, 2, 8, in.as.op.src2.value); // bytes remaining
		    fprintf(out, "   mov x3, #0\n"); // offset = 0
		    fprintf(out, "1:\n");
		    fprintf(out, "   ldrb w6, [x1, x3]\n"); // load byte from src
		    fprintf(out, "   strb w6, [x0, x3]\n"); // store byte to dst
		    fprintf(out, "   add x3, x3, #1\n"); // offset++
		    fprintf(out, "   cmp x3, x2\n"); // check if done
		    fprintf(out, "   b.lt 1b\n");  // loop if offset < size
		} break;
		case INS_LSTR: {
			assert(in.as.op.src1.type == argument_string);
			string str = ir.strings.ptr[in.as.op.src1.offset];

			load_immediate(out, 0, 8, str.len);
			store_arg(out, 0, in.as.op.dst, 0);

	    	fprintf(out, "   adrp x0, .LC%zu@PAGE\n", in.as.op.src1.offset);
			fprintf(out, "   add x0, x0, .LC%zu@PAGEOFF\n", in.as.op.src1.offset);

			store_arg(out, 0, in.as.op.dst, 8);
		} break;
		case INS_STORE: {
			load_arg(out, 0, in.as.op.src1);
			store_arg(out, 0, in.as.op.dst, 0);
		} break;
		case INS_ADD: {
			load_arg(out, 0, in.as.op.src1);
			load_arg(out, 1, in.as.op.src2);
			fprintf(out, "   add x0, x0, x1\n");
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
