typedef struct {
	string identifier;
	int pos;
} label;

typedef struct {
	int len;
	int cap;
	label* ptr;
} label_list;

#define INVALID_RET (uint64_t)-1
#define STACK_CAP (1024)

uint8_t* globals;
uint64_t stack[STACK_CAP];
uint64_t* stack_ptr = &stack[STACK_CAP-1];

// x0 -> return val
// x29 -> frame ptr
// x30 -> return addr
uint64_t regs[32];

strings string_literals;
label_list labels = {0};

uint8_t load8(argument src) {
	ASSERT(src.size>=1);
	switch(src.type) { 
	case argument_type_local: return *(uint8_t*)((uint8_t*)(uintptr_t)regs[29] - src.as.offset); 
	case argument_type_global: return *(uint8_t*)(globals + src.as.offset); 
	case argument_type_literal: return src.as.value; 
	default: unreachable; 
	} 
}

uint32_t load32(argument src) {
    ASSERT(src.size>=4);
    switch(src.type) { 
    case argument_type_local: return *(uint32_t*)((uint8_t*)(uintptr_t)regs[29] - src.as.offset); 
    case argument_type_global: return *(uint32_t*)(globals + src.as.offset);
    case argument_type_literal: return src.as.value; 
    default: unreachable; 
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
	default: unreachable; 
	} 
}

void* argument_location(argument dst) {
    switch(dst.type) {
    case argument_type_local: return (uint8_t*)(uintptr_t)regs[29] - dst.as.offset;
    case argument_type_global: return globals + dst.as.offset;
    default: unreachable;
    }
}

uint64_t load(argument src) {
	switch(src.size) {
	case 1: return load8(src);
	case 4: return load32(src);
	case 8: return load64(src);
	default: unreachable;
	}
}

void store(argument dst, uint64_t value) {
	if (dst.type == argument_type_vreg) {
		regs[dst.as.vreg] = value;
	} else {
		void* dst_ptr = argument_location(dst);
		switch(dst.size) {
		case 1: *(uint8_t*)dst_ptr = (uint8_t)value; break;
		case 4: *(uint32_t*)dst_ptr = (uint32_t)value; break;
		case 8: *(uint64_t*)dst_ptr = (uint64_t)value; break;
		default: unreachable;
		}
	}
}

int labelpos(string identifier) {
	for (int i = 0; i < labels.len; ++i) {
		label l = labels.ptr[i];
		if(string_eq(l.identifier, identifier)) return l.pos;
	}
	unreachable;
}

void simulate(intermediate_representation ir) {
	fprintf(stderr, "info: running program\n");
	fprintf(stderr, "info: instruction count: %d\n", ir.instructions.len);

	string_literals = ir.string_literals;

	int pc = -1;
	for (int i = 0; i < ir.instructions.len; ++i) {
		instruction ins = ir.instructions.ptr[i];
		if (ins.type == ins_label) {
			array_append(&labels, ((label){ins.as.label, i}));
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
    		int reg = 0;
			for (int i = 0; i < ins.as.fcall.argc; ++i) {
				if (i < 8) {
					if (ins.as.fcall.args[i].size <= PTR_SIZE) {
						regs[reg++] = load(ins.as.fcall.args[i]);
					} else if (ins.as.fcall.args[i].size <= 2*PTR_SIZE) {
						// TODO: find actual field size
						regs[reg++] = load(argument_field(ins.as.fcall.args[i], 0, PTR_SIZE));
						regs[reg++] = load(argument_field(ins.as.fcall.args[i], PTR_SIZE, PTR_SIZE));
					} else {
						regs[reg++] = (uintptr_t)argument_location(ins.as.fcall.args[i]);
					}
		        } else {
					uint64_t value;
					if (ins.as.fcall.args[i].size <= PTR_SIZE)
		            	value = load(ins.as.fcall.args[i]);
		            else
		            	value = (uint64_t)argument_location(ins.as.fcall.args[i]);
		            uint64_t* fp_words = (uint64_t*)(uintptr_t)regs[29];
		            fp_words[i - 8] = value;
		        }
			}
			regs[30] = (uint64_t)pc;
			pc = labelpos(ins.as.fcall.identifier);
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
			    if (idx < 8) {
			    	if (ins.as.op.src1.size <= PTR_SIZE) {
						store(ins.as.op.dst, regs[idx]);
					} else if (ins.as.op.src1.size <= 2*PTR_SIZE) {
						// TODO: find actual field size
						store(argument_field(ins.as.op.dst, 0, PTR_SIZE), regs[idx]);
						store(argument_field(ins.as.op.dst, PTR_SIZE, PTR_SIZE), regs[idx+1]);
					} else unreachable;
			    } else {
			        uint64_t* fp_words = (uint64_t*)(uintptr_t)regs[29];
			        store(ins.as.op.dst, fp_words[idx - 8]);
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
				default: unreachable;
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
				default: unreachable;
				}
			} break;

			case op_copy: {
				ASSERT(ins.as.op.src2.type == argument_type_literal);
				memcpy(argument_location(ins.as.op.dst), argument_location(ins.as.op.src1), ins.as.op.src2.as.value);
			} break;

			default:
				printf("unimplemented op: %d\n", ins.as.op.type);
				unreachable;
			}
		} break;

		case ins_ret: {
			if (ins.as.ret.type != argument_type_none) {
		        regs[0] = load(ins.as.ret);
		    }
		} break;

		case ins_jmp: {
			pc = labelpos(ins.as.jmp.label);
			continue;
		} break;

		case ins_jmp_ifnot: {
				int cond = load(ins.as.jmpifnot.cond);
				if (!cond) {
					pc = labelpos(ins.as.jmpifnot.label);
					continue;
				} 
		} break;

		default: 
			printf("unimplemented ins: %d\n", ins.type);
			unreachable
		}

		pc++;
	}
	fprintf(stderr, "info: exitcode: %llu\n", regs[0]);
}