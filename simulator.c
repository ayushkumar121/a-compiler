#include <unistd.h>

typedef struct {
	string identifier;
	int pos;
} label_pos;

typedef struct {
	int len;
	int cap;
	label_pos* ptr;
} label_pos_list;

#define INVALID_RET (uint64_t)-1
#define STACK_CAP (1024)
#define MAX_ARGS 16

uint8_t* globals;
uint64_t stack[STACK_CAP];
uint64_t* stack_ptr = &stack[STACK_CAP-1];

// x0 -> return val
// x29 -> frame ptr
// x30 -> return addr
uint64_t regs[32];

strings string_literals;
label_pos_list labels = {0};

uint8_t load8(argument src) {
	ASSERT(src.size>=1);
	switch(src.type) { 
	case argument_type_local: return *(uint8_t*)((uint8_t*)(uintptr_t)regs[29] - src.as.offset); 
	case argument_type_global: return *(uint8_t*)(globals + src.as.offset); 
	case argument_type_literal: return src.as.value; 
	default: unreachable(); 
	} 
}

uint32_t load32(argument src) {
    ASSERT(src.size>=4);
    switch(src.type) { 
    case argument_type_local: return *(uint32_t*)((uint8_t*)(uintptr_t)regs[29] - src.as.offset); 
    case argument_type_global: return *(uint32_t*)(globals + src.as.offset);
    case argument_type_literal: return src.as.value; 
    default: unreachable(); 
    } 
}

uint64_t load64(argument src) {
	ASSERT(src.size >= 8);
	switch(src.type) { 
	case argument_type_vreg: return regs[src.as.vreg];
	case argument_type_local: return *(uint64_t*)((uint8_t*)(uintptr_t)regs[29] - src.as.offset); 
	case argument_type_global: return *(uint64_t*)(globals + src.as.offset); 
	case argument_type_literal: return src.as.value; 
	case argument_type_string: return (uint64_t)string_literals.ptr[src.as.index].ptr; 
	default: unreachable(); 
	} 
}

void* argument_location(argument dst) {
    switch(dst.type) {
    case argument_type_local: return (uint8_t*)(uintptr_t)regs[29] - dst.as.offset;
    case argument_type_global: return globals + dst.as.offset;
    default: unreachable();
    }
}

uint64_t load(argument src) {
	switch(src.size) {
	case 1: return load8(src);
	case 4: return load32(src);
	case 8: return load64(src);
	default: unreachable();
	}
}

void store(argument dst, uint64_t value) {
	if(dst.size == 0) return;
	if (dst.type == argument_type_vreg) {
		regs[dst.as.vreg] = value;
	} else {
		void* dst_ptr = argument_location(dst);
		switch(dst.size) {
		case 1: *(uint8_t*)dst_ptr = (uint8_t)value; break;
		case 4: *(uint32_t*)dst_ptr = (uint32_t)value; break;
		case 8: *(uint64_t*)dst_ptr = (uint64_t)value; break;
		default: unreachable();
		}
	}
}

int find_label_pos(string identifier) {
	for (int i = 0; i < labels.len; ++i) {
		label_pos l = labels.ptr[i];
		if(string_eq(l.identifier, identifier)) return l.pos;
	}
	unreachable();
}

int find_builtin(string identifier) {
	static_assert(builtin_count == 2, "This function needs updating");
	if (string_eq(identifier, sv("print"))) return builtin_print;
	else if (string_eq(identifier, sv("exit"))) return builtin_exit;
	return -1;
}

void execute_builtin(builtin id) {
	switch(id) {
		case builtin_print: {
			write(1, (char*)regs[1], (int)regs[0]);
		} break;
		case builtin_exit: {
			exit(regs[0]);
		} break;
		case builtin_count: unreachable();
	}

	regs[0] = 0;
}

void simulate(intermediate_representation ir) {
	fprintf(stderr, "info: running program\n");
	fprintf(stderr, "info: instruction count: %d\n", ir.instructions.len);

	string_literals = ir.string_literals;

	int pc = -1;
	for (int i = 0; i < ir.instructions.len; ++i) {
		instruction ins = ir.instructions.ptr[i];
		if (ins.type == ins_label) {
			array_append(&labels, ((label_pos){ins.as.label, i}));
			if (string_eq(ins.as.label, sv("main"))) pc = i;
		}
	}

	if (pc == -1) {
		fprintf(stderr, "error: no main function found\n");
		exit(1);
	}
	fprintf(stderr, "info: main at pc: %d\n", pc);

	regs[29] = (uint64_t)(uintptr_t)stack_ptr;
	regs[30] = (uint64_t)ir.instructions.len;

	while (pc < ir.instructions.len) {
		instruction ins = ir.instructions.ptr[pc];

		// printf("ins.type=%d pc=%d\n", ins.type, pc);

		switch(ins.type) {
		case ins_label: break;
		
		case ins_func_start: {
			// push frame_ptr and return_addr
		    *stack_ptr = (uint64_t)(uintptr_t)regs[29]; stack_ptr--;
		    *stack_ptr = regs[30]; stack_ptr--;

		    // update frame_addr
		    regs[29] = (uint64_t)(uintptr_t)(stack_ptr);

		    // allocate space for locals (in 8-byte slots)
		    int slots = (ins.as.frame->size + 7) / 8;
		    stack_ptr -= slots;
		} break;

		case ins_func_end: {
    		// deallocate locals
			int slots = (ins.as.frame->size + 7) / 8;
    		stack_ptr += slots;
		    // pop return address
		    stack_ptr++; regs[30] = *stack_ptr;
		    // pop old frame pointer
		    stack_ptr++; regs[29] = *stack_ptr;
		    pc = regs[30];
		    continue;
		} break;

		case ins_func_call: {
			if (regs[30] == (uint64_t)pc) {
			    store(ins.as.fcall.dst, regs[0]);
		        regs[30] = INVALID_RET;
		        break;
    		}

    		ASSERT(ins.as.fcall.argc <= MAX_ARGS);
    		int slot_index = 0;
			for (int i = 0; i < ins.as.fcall.argc; ++i) {
				argument arg = ins.as.fcall.args[i];
				if (arg.size <= 8) {
					regs[slot_index++] = load(arg);
				} else if (arg.size <= 16) {
					// TODO: find actual field size
					regs[slot_index++] = load(argument_field(arg, 0, PTR_SIZE));
					regs[slot_index++] = load(argument_field(arg, PTR_SIZE, PTR_SIZE));
				} else {
					regs[slot_index++] = (uintptr_t)argument_location(ins.as.fcall.args[i]);
				}
			}

			regs[30] = (uint64_t)pc;
			int fid = find_builtin(ins.as.fcall.identifier);
			if (fid != -1) {
				execute_builtin(fid);
			} else {
				pc = find_label_pos(ins.as.fcall.identifier);
			}
			continue;
		} break;

		case ins_binop: {
			switch(ins.as.op.type) {
			case op_add: {
				store(ins.as.op.dst, load(ins.as.op.src1) + load(ins.as.op.src2));
			} break;

			case op_sub: {
				store(ins.as.op.dst, load(ins.as.op.src1) - load(ins.as.op.src2));
			} break;

			case op_mul: {
				store(ins.as.op.dst, load(ins.as.op.src1) * load(ins.as.op.src2));
			} break;

			case op_div: {
				store(ins.as.op.dst, load(ins.as.op.src1) / load(ins.as.op.src2));
			} break;

			case op_madd: {
				store(ins.as.op.dst, load64(ins.as.op.dst) + load(ins.as.op.src1) * load(ins.as.op.src2));
			} break;

			case op_addrof: {
				ASSERT(ins.as.op.dst.size == PTR_SIZE);
				store(ins.as.op.dst, (uint64_t)argument_location(ins.as.op.src1));
			} break;

			case op_store: {
				ASSERT(ins.as.op.dst.size <= PTR_SIZE);
				ASSERT(ins.as.op.src1.size <=  ins.as.op.dst.size);
				store(ins.as.op.dst, load(ins.as.op.src1));
			} break;

			case op_load_param: {
				ASSERT(ins.as.op.src1.size <=  ins.as.op.dst.size);
				ASSERT(ins.as.op.src1.type ==  argument_type_param);

			    int idx = ins.as.op.src1.as.index;
			    argument dst = ins.as.op.dst;
		    	if (ins.as.op.src1.size <= 8) {
					store(ins.as.op.dst, regs[idx]);
				} else if (ins.as.op.src1.size <= 16) {
					// TODO: find actual field size
					store(argument_field(dst, 0, PTR_SIZE), regs[idx]);
					store(argument_field(dst, PTR_SIZE, PTR_SIZE), regs[idx+1]);
				} else {
					memcpy(argument_location(ins.as.op.dst), (void*)regs[idx], ins.as.op.src1.size);
				}
			} break;

			case op_load_indirect: {
				ASSERT(ins.as.op.dst.size <= PTR_SIZE);
				ASSERT(ins.as.op.src2.type != argument_type_none);

				void* src = (void*)load64(ins.as.op.src1);
				void* dst = argument_location(ins.as.op.dst);

				switch(load(ins.as.op.src2)) {
				case 1: *(uint8_t*)dst = *(uint8_t*)src; break;
				case 4: *(uint32_t*)dst = *(uint32_t*)src; break;
				case 8: *(uint64_t*)dst = *(uint64_t*)src; break;
				default: unreachable();
				}
			} break;

			case op_store_indirect: {
				ASSERT(ins.as.op.dst.size <= PTR_SIZE);
				ASSERT(ins.as.op.src2.type != argument_type_none);

				uint64_t value = load(ins.as.op.src1);
				void* dst = (void*)load64(ins.as.op.dst);

				switch(load(ins.as.op.src2)) {
				case 1: *(uint8_t*)dst = value; break;
				case 4: *(uint32_t*)dst = value; break;
				case 8: *(uint64_t*)dst = value; break;
				default: unreachable();
				}
			} break;

			case op_copy: {
				ASSERT(ins.as.op.src2.type == argument_type_literal);
				memcpy(argument_location(ins.as.op.dst), argument_location(ins.as.op.src1), ins.as.op.src2.as.value);
			} break;

			default:
				printf("unimplemented op: %d\n", ins.as.op.type);
				unreachable();
			}
		} break;

		case ins_ret: {
			if (ins.as.ret.type != argument_type_none) {
		        regs[0] = load(ins.as.ret);
		    }
		} break;

		case ins_jmp: {
			pc = find_label_pos(ins.as.jmp.label);
			continue;
		} break;

		case ins_jmp_ifnot: {
				int cond = load(ins.as.jmpifnot.cond);
				if (!cond) {
					pc = find_label_pos(ins.as.jmpifnot.label);
					continue;
				} 
		} break;

		default: 
			printf("unimplemented ins: %d\n", ins.type);
			unreachable();
		}

		pc++;
	}
	fprintf(stderr, "info: exitcode: %d\n", (int)regs[0]);
}
