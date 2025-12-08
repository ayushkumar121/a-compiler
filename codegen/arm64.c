int vreg_mapping[R_Count] = {
	[R0] = 9, //x4
	[R1] = 10, //x5
	[R2] = 11, //x6
	[R3] = 12, //x7
};

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
	ASSERT(src.size <= PTR_SIZE);
	char reg_size = src.size <= 4 ? 'w' : 'x';

	switch(src.type) {
	case argument_type_literal:
		load_immediate(out, reg, src.size, src.as.value);
		break;

	case argument_type_vreg:
		ASSERT(reg != vreg_mapping[src.as.vreg]);
    	fprintf(out, "   mov %c%d, %c%d\n", reg_size, reg, reg_size, vreg_mapping[src.as.vreg]);
		break;

	case argument_type_local:
		fprintf(out, "   ldr %c%d, [x29, #-%d]\n", reg_size, reg, src.as.offset);
		break;

	case argument_type_global:
	    fprintf(out, "   adrp x9, _globals@PAGE\n");
	    fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
	    fprintf(out, "   ldr %c%d, [x9, #%d]\n", reg_size, reg, src.as.offset);
		break;

	case argument_type_string:
    	fprintf(out, "   adrp x%d, .S%d@PAGE\n", reg, src.as.index);
		fprintf(out, "   add x%d, x%d, .S%d@PAGEOFF\n", reg, reg, src.as.index);
		break;

	default: unreachable;
	}
}

void store_arg(FILE* out, int reg, argument dst) {
	ASSERT(dst.size <= PTR_SIZE);
	char reg_size = dst.size <= 4? 'w':'x';
	switch(dst.type) {
	case argument_type_vreg:
		ASSERT(reg != vreg_mapping[dst.as.vreg]);
    	fprintf(out, "   mov %c%d, %c%d\n", reg_size, vreg_mapping[dst.as.vreg], reg_size, reg);
		break;

	case argument_type_local:
		fprintf(out, "   str %c%d, [x29, #-%d]\n", reg_size, reg, dst.as.offset);
		break;

	case argument_type_global:
    	fprintf(out, "   adrp x9, _globals@PAGE\n");
		fprintf(out, "   add x9, x9, _globals@PAGEOFF\n");
		fprintf(out, "   str %c%d, [x9, #%d]\n", reg_size, reg, dst.as.offset);
		break;
		
	default: unreachable;
	}
}

void load_addr(FILE* out, int reg, argument src) {
	switch(src.type) {
	case argument_type_local:
		fprintf(out, "   sub x%d, x29, #%d\n", reg, src.as.offset);
		break;

	case argument_type_global:
		fprintf(out, "   adrp x0, _globals@PAGE\n");
		fprintf(out, "   add x0, x0, _globals@PAGEOFF\n");
		fprintf(out, "   add x%d, x0, #%d\n", reg, src.as.offset);
		break;

	default: unreachable;
	}
}

void load_param(FILE* out, argument src, argument dst) {	
	ASSERT(dst.type == argument_type_local);
	ASSERT(src.type == argument_type_param);

	int index = src.as.index;
	char reg_size = dst.size <= 4? 'w':'x';
	if (index < 8) {
		if (src.size <= PTR_SIZE) {
        	fprintf(out, "    str %c%d, [x29, #-%d]\n",  reg_size, src.as.index, dst.as.offset);
    	} else if (src.size <= 2*PTR_SIZE) {
			// TODO: find actual field size
    		store_arg(out, src.as.index, argument_field(dst, 0, PTR_SIZE));
    		store_arg(out, src.as.index+1, argument_field(dst, PTR_SIZE, PTR_SIZE));
    	} else unreachable;
    } else {
        int slot = index - 8;
        fprintf(out, "    ldr %c0, [x29, #%d]\n", reg_size,  16 + slot*8);
        fprintf(out, "    str %c0, [x29, #-%d]\n",reg_size, dst.as.offset);
    }
}

void codegen_for_arm64_macos(intermediate_representation ir, string asm_path) {
	fprintf(stderr, "info: generating %.*s\n", sarg(asm_path));
	FILE* out = fopen(asm_path.ptr, "w+");

	fprintf(out, ".text\n");
	fprintf(out, ".global _start\n\n");
	fprintf(out, "_start:\n");
	fprintf(out, "   bl _main\n");
	fprintf(out, "   b _exit\n");

	for (int i = 0; i <builtin_count; ++i){
		switch(i) {
		case builtin_print: 
			fprintf(out, "_print:\n");
			fprintf(out, "   mov x2, x0\n");
			fprintf(out, "   mov x0, #1\n"); // SYS_OUT
			fprintf(out, "   movz x16, #0x4\n");
			fprintf(out, "   movk x16, #0x200, lsl #16\n");
			fprintf(out, "   svc #0\n");
			fprintf(out, "   ret\n");
			break;
		case builtin_exit:
			fprintf(out, "_exit:\n");
			fprintf(out, "   movz x16, #0x1\n");
			fprintf(out, "   movk x16, #0x200, lsl #16\n");
			fprintf(out, "   svc #0\n");
			fprintf(out, "   b .\n");
		 	break;
		case builtin_count: break;
		}
	}

	fprintf(out, "\n\n");

	size_t frame_size;
	for (int i=0; i<ir.instructions.len; i++) {
		instruction ins = ir.instructions.ptr[i];
		switch(ins.type){
		case ins_label:
			fprintf(out, "; ins_label\n");
			fprintf(out, "_"sfmt":\n", sarg(ins.as.label));
			break;

		case ins_func_start:
			fprintf(out, "; ins_func_start\n");
			fprintf(out, "   stp x29, x30, [sp, #-16]!\n");
			fprintf(out, "   mov x29, sp\n");
			frame_size = align(ins.as.frame->size, 16);
			fprintf(out, "   sub sp, sp, #%zu\n", frame_size);
			break;

		case ins_func_end:
			fprintf(out, "; ins_func_end\n");
			fprintf(out, "   add sp, sp, #%zu\n", frame_size);
			fprintf(out, "   ldp x29, x30, [sp], #16\n");
			fprintf(out, "   ret\n");
			break;

		case ins_func_call: 
			fprintf(out, "; ins_func_call\n");
			int reg = 0;
			for (int i = 0; i < ins.as.fcall.argc; ++i) {
				if (i < 8) {
					if (ins.as.fcall.args[i].size <= PTR_SIZE) {
						load_arg(out, reg++, ins.as.fcall.args[i]);
					} else if (ins.as.fcall.args[i].size <= 2*PTR_SIZE) {
						// TODO: find actual field size
						load_arg(out, reg++, argument_field(ins.as.fcall.args[i], 0, PTR_SIZE));
						load_arg(out, reg++, argument_field(ins.as.fcall.args[i], PTR_SIZE, PTR_SIZE));
					} else {
						load_addr(out, reg++, ins.as.fcall.args[i]);
					}
		        } else {
		            int slot = i - 8;
		            int offset = 16 + slot * 8;
		            if (ins.as.fcall.args[i].size <= PTR_SIZE)
		            	load_arg(out, 0, ins.as.fcall.args[i]);
		            else
		            	load_addr(out, 0, ins.as.fcall.args[i]);
		            fprintf(out, "   str x0, [x29, #-%d]\n", offset);
		        }
			}
			fprintf(out, "   bl _"sfmt"\n", sarg(ins.as.fcall.identifier));
			store_arg(out, 0, ins.as.fcall.dst);
			break;

		case ins_ret:
			fprintf(out, "; ins_ret\n");
			load_arg(out, 0, ins.as.ret);
			break;

		case ins_binop: {
			switch(ins.as.op.type) {
			case op_add:
				fprintf(out, "; op_add\n");
				load_arg(out, 0, ins.as.op.src1);
				load_arg(out, 1, ins.as.op.src2);
				fprintf(out, "   add x0, x0, x1\n");
				store_arg(out, 0, ins.as.op.dst);
				break;

			case op_sub:
				fprintf(out, "; op_sub\n");
				load_arg(out, 0, ins.as.op.src1);
				load_arg(out, 1, ins.as.op.src2);
				fprintf(out, "   sub x0, x0, x1\n");
				store_arg(out, 0, ins.as.op.dst);
				break;

			case op_mul:
				fprintf(out, "; op_mul\n");
				load_arg(out, 0, ins.as.op.src1);
				load_arg(out, 1, ins.as.op.src2);
				fprintf(out, "   mul x0, x0, x1\n");
				store_arg(out, 0, ins.as.op.dst);
				break;

			case op_div:
				fprintf(out, "; op_mul\n");
				load_arg(out, 0, ins.as.op.src1);
				load_arg(out, 1, ins.as.op.src2);
				fprintf(out, "   sdiv x0, x0, x1\n");
				store_arg(out, 0, ins.as.op.dst);
				break;

			case op_madd:
				fprintf(out, "; op_madd\n");
			    load_arg(out, 0, ins.as.op.dst);
			    load_arg(out, 1, ins.as.op.src1);
			    load_arg(out, 2, ins.as.op.src2);
			    fprintf(out, "   madd x0, x1, x2, x0\n");
			    store_arg(out, 0, ins.as.op.dst);
				break;

			case op_addrof:
				fprintf(out, "; op_addrof\n");
				ASSERT(ins.as.op.dst.size == PTR_SIZE);
				load_addr(out, 0, ins.as.op.src1);
				store_arg(out, 0, ins.as.op.dst);
				break;

			case op_load_param:
				fprintf(out, "; op_load_param\n");
				load_param(out, ins.as.op.src1, ins.as.op.dst);
				break;

			case op_load_indirect:
				fprintf(out, "; op_load_indirect\n");
				ASSERT(ins.as.op.dst.size <= PTR_SIZE);
				ASSERT(ins.as.op.src2.type != argument_type_none);

				load_arg(out, 0, ins.as.op.src1);
			    fprintf(out, "   ldr %c0, [x0]\n", ins.as.op.dst.size <= 4 ? 'w' : 'x');
				store_arg(out, 0, ins.as.op.dst);
				break;

			case op_store:
				fprintf(out, "; op_store\n");
				load_arg(out, 0, ins.as.op.src1);
				store_arg(out, 0, ins.as.op.dst);
				break;

			case op_store_indirect:
				fprintf(out, "; op_store_indirect\n");
				ASSERT(ins.as.op.dst.size <= PTR_SIZE);
				ASSERT(ins.as.op.src2.type != argument_type_none);

				load_arg(out, 0, ins.as.op.src1);
    			load_arg(out, 1, ins.as.op.dst);

				fprintf(out, "   str %c0, [x1]\n", ins.as.op.src1.size <= 4 ? 'w' : 'x');
				break;

			case op_copy:
			    fprintf(out, "; op_copy\n");
			    ASSERT(ins.as.op.src2.type == argument_type_literal);
			    
			    int size = ins.as.op.src2.as.value;
			    
			    load_addr(out, 0, ins.as.op.src1);  // src address -> x0
			    load_addr(out, 1, ins.as.op.dst);   // dst address -> x1
			    load_immediate(out, 2, 8, size);     // size -> x2
			    fprintf(out, "   mov x3, #0\n");     // offset = 0
			    fprintf(out, "1:\n");                // Loop label
			    fprintf(out, "   ldrb w4, [x0, x3]\n");  // Load byte from src
			    fprintf(out, "   strb w4, [x1, x3]\n");  // Store byte to dst
			    fprintf(out, "   add x3, x3, #1\n");     // offset++
			    fprintf(out, "   cmp x3, x2\n");         // Compare offset with size
			    fprintf(out, "   b.lt 1b\n");         // Loop if offset < size
			    break;

			default:{
				printf("unimplemented op %d\n", ins.as.op.type);
				unreachable;
			}
			}
		} break;

		case ins_jmp:
			fprintf(out, "   b _"sfmt"\n", sarg(ins.as.jmp.label));
			break;

		case ins_jmp_ifnot:
			load_arg(out, 0, ins.as.jmpifnot.cond); 
			fprintf(out, "   cmp x0, #0\n");
			fprintf(out, "   beq _"sfmt"\n", sarg(ins.as.jmpifnot.label));
			break;

		default: {
			printf("unimplemented ins %d\n", ins.type);
			unreachable;
		}
		}
	}

	if (ir.string_literals.len > 0) {
		fprintf(out, ".section __TEXT,__cstring,cstring_literals\n");
		for (int i=0; i<ir.string_literals.len; i++) {
			fprintf(out, ".S%d:\n", i);
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
