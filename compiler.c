typedef enum {
	argument_none,
	argument_literal,
	argument_param,
	argument_local,
	argument_global,
	argument_string,
} argument_type;

typedef enum {
	argument_value_int,
	argument_value_ptr,
	argument_value_struct,
} argument_value_type;

typedef struct {
	argument_type type;
	argument_value_type value_type;
	string name;
	size_t offset;
	size_t size;
	size_t alignment;
	size_t value;
} argument;

#define argument_error (argument){.type=argument_none}

typedef struct {
	int len;
	int cap;
	argument* ptr;
} argument_list;

typedef enum {
	INS_LEA,
	INS_LSTR,
    INS_STORE,
    INS_ADD,
    INS_SUB,
    INS_MUL,
    INS_DIV,
    INS_CALL,
    INS_RET,
    INS_LABEL,
    INS_BRANCH,
    INS_CMP,
    INS_COPY,
    INS_SYSCALL,
    INS_FUNC_START,
    INS_FUNC_END,
    INS_NOP,
} instruction_type;

typedef struct {
	size_t offset;
	size_t size;
	size_t alignment;
	string identifier;
} frame_local;

typedef struct {
	size_t size;
	size_t temp_count;
	// for debugging
	struct {
		int len;
		int cap;
		frame_local* ptr;
	} locals;
} frame;

typedef struct {
	instruction_type type;
	string label;
	union {
		struct {argument dst, src1, src2;} op;
		struct {argument_list params; frame* frame;} func;
		struct {argument dst; argument_list params;} call;
	} as;
} instruction;

typedef struct {
	int len;
	int cap;
	instruction* ptr;
} instruction_list;

typedef struct {
	strings strings;
	instruction_list instructions;
} intermediate_representation;

typedef struct symbol symbol;

typedef enum {
	symbol_type_param,
	symbol_type_local,
	symbol_type_global,
	symbol_type_function,
	symbol_type_struct,
} symbol_type;

typedef struct symbol {
	symbol_type symbol_type;
	string identifier;
	size_t offset;
	size_t alignment;
	size_t size;
	type type; 
	symbol* next;
	symbol* prev;
} symbol;

typedef struct {
	symbol* first;
	symbol* last;
} symbol_table;

string value_type_to_string(argument_value_type value_type) {
	switch(value_type) {
	case argument_value_int: return sv("int");
	case argument_value_ptr: return sv("ptr");
	case argument_value_struct: return sv("struct");
	}
}

string argument_to_string(argument arg) {
	switch(arg.type) {
	case argument_none: 
		return tsprintf("<empty>");
	case argument_string: 
		return tsprintf("string(index=%zu)", arg.offset);
	case argument_literal: 
		return tsprintf("literal(value_type=%.*s size=%zu alignment=%zu value=%zu)", 
			string_arg(value_type_to_string(arg.value_type)), arg.size, arg.alignment, arg.value);
	case argument_param: 
		return tsprintf("param(name=%.*s value_type=%.*s offset=%zu size=%zu alignment=%zu)", 
			string_arg(arg.name), string_arg(value_type_to_string(arg.value_type)), arg.offset, arg.size, arg.alignment); break;
	case argument_local: 
		return tsprintf("local(name=%.*s value_type=%.*s offset=%zu size=%zu alignment=%zu)", 
			string_arg(arg.name), string_arg(value_type_to_string(arg.value_type)), arg.offset, arg.size, arg.alignment); break;
	case argument_global:
		return tsprintf("global(name=%.*s value_type=%.*s offset=%zu size=%zu alignment=%zu)", 
			string_arg(arg.name), string_arg(value_type_to_string(arg.value_type)), arg.offset, arg.size, arg.alignment); break;
	}
}

string frame_to_string(frame* frame) {
	assert(frame != NULL);
	string str = tsprintf("{\n\t\tsize=%zu\n\t\tlocals={\n", frame->size);
	
	for (int i = 0; i < frame->locals.len; i++) {
		frame_local local = frame->locals.ptr[i];
		str = tconcat(str, tsprintf("\t\t\t%.*s(%zu)=%zu\n", 
			string_arg(local.identifier), local.size, local.offset));
	}
	
	return tconcat(str, sv("\t\t}\n\t}"));
}

void print_instruction(instruction in) {
	switch(in.type) {
    case INS_LEA: printf("INS_LEA"); break;
    case INS_LSTR: printf("INS_LSTR"); break;
    case INS_STORE: printf("INS_STORE"); break;
    case INS_ADD: printf("INS_ADD"); break;
    case INS_SUB: printf("INS_SUB"); break;
    case INS_MUL: printf("INS_MUL"); break;
    case INS_DIV: printf("INS_DIV"); break;
    case INS_CALL: printf("INS_CALL"); break;
    case INS_RET: printf("INS_RET"); break;
    case INS_LABEL: printf("INS_LABEL"); break;
    case INS_BRANCH: printf("INS_BRANCH"); break;
    case INS_CMP: printf("INS_CMP"); break;
    case INS_COPY: printf("INS_COPY"); break;
    case INS_SYSCALL: printf("INS_SYSCALL"); break;
    case INS_FUNC_START: printf("INS_FUNC_START"); break;
    case INS_FUNC_END: printf("INS_FUNC_END"); break;
    case INS_NOP: printf("INS_NOP"); break;
	}

	printf(" {\n");
	if (in.type == INS_FUNC_START) {
		printf("\tlabel=%.*s\n\tframe=%.*s\n\tparams={\n", 
			string_arg(in.label), string_arg(frame_to_string(in.as.func.frame)));
		for (int i=0; i<in.as.func.params.len; i++) {
			printf("\t\t%.*s\n", string_arg(argument_to_string(in.as.func.params.ptr[i]))); 
		}
		printf("\t}\n");
	} else if (in.type == INS_FUNC_END) {
		/* Print nothing */
	}else if (in.type == INS_CALL) {
		printf("\tlabel=%.*s\n\tdst=%.*s\n\tparams={\n", 
			string_arg(in.label), string_arg(argument_to_string(in.as.call.dst))); 
		for (int i=0; i<in.as.call.params.len; i++) {
			printf("\t\t%.*s\n", string_arg(argument_to_string(in.as.call.params.ptr[i]))); 
		}
		printf("\t}\n");
	} else if (in.type == INS_LABEL) {
		printf("\tlabel=%.*s\n", string_arg(in.label));
	} else if (in.type == INS_RET) {
		printf("\tvalue=%.*s\n", string_arg(argument_to_string(in.as.op.dst)));
	} else {
		printf("\tdst=%.*s\n\tsrc1=%.*s\n\tsrc2=%.*s\n", 
			string_arg(argument_to_string(in.as.op.dst)),
			string_arg(argument_to_string(in.as.op.src1)),
			string_arg(argument_to_string(in.as.op.src2)));
	}
	printf("}\n");
}

void print_ir(intermediate_representation ir) {
	for (int i=0; i<ir.instructions.len; i++)
		print_instruction(ir.instructions.ptr[i]);
	printf("STRINGS {\n");
	for (int i=0; i<ir.strings.len; i++) {
		printf("\t%d=%.*s\n", i, string_arg(ir.strings.ptr[i]));
	}
	printf("}\n");
}

instruction_list instructions = {0};
symbol_table symbols = {0};
strings string_literals = {0};

void print_symbol_table() {
	symbol* symbl = symbols.first;
	while(symbl != NULL) {
		print(symbl->identifier);
		print(sv(":"));
		println(type_to_string(symbl->type));
		symbl = symbl->next;
	}
}

symbol* symbol_lookup(string identifier) {
	if(symbols.last != NULL) {
		symbol* symbl = symbols.last;
		while(symbl != NULL) {
			if (string_eq(symbl->identifier, identifier)) {
				return symbl;
			} 
			symbl = symbl->prev;
		}
	}
	return NULL;
}

void symbol_add(symbol_type symbol_type, string identifier, type type, size_t offset, size_t size, size_t alignment) {
	symbol* symbl = malloc(sizeof(symbol));
	symbl->type = type;
	symbl->identifier = identifier;
	symbl->offset = offset;
	symbl->size = size;
	symbl->alignment = alignment;
	symbl->symbol_type = symbol_type;
	symbl->prev = NULL;
	symbl->next = NULL;

	if (symbols.last == NULL) {
		symbols.first = symbl;
		symbols.last = symbl;
	} else {
		symbols.last->next = symbl;
		symbl->prev = symbols.last;
		symbols.last = symbl;
	}
}

argument frame_allocate(argument_type arg_type, frame* fr, string identifier, size_t size, size_t alignment) {
	size_t offset = fr->size;
	fr->size += align(size, alignment);
	frame_local local = {.identifier=identifier, .offset=offset, .size=size, .alignment=alignment};
	array_append(&fr->locals, local);
	argument_value_type arg_value_type = (size <= 8)? 
		argument_value_int: argument_value_struct;
	return (argument){.type=arg_type, .value_type=arg_value_type, .name=identifier, .offset=offset, .size=size, .alignment=alignment};
}

argument frame_allocate_temp(frame* fr, size_t size, size_t alignment) {
	return frame_allocate(argument_local, fr, tsprintf("t%zu", (fr->temp_count)++), size, alignment);
}

size_t alignment_of_type(type type);

size_t size_of_type(type type) {
	switch(type.type) {
	case type_primitive: {
		switch(type.as.primitive) {
		case primitive_void: return 0;
		case primitive_byte:
		case primitive_ubyte: return 1;
		case primitive_short:
		case primitive_ushort: return 2;
		case primitive_int:
		case primitive_uint: return 4;
		case primitive_long: 
		case primitive_ulong: return 8;
		case primitive_float: return 4;
		case primitive_double: return 8;
		case primitive_string: return 16;
		case primitive_none: unreachable;
		}
	}
	case type_wrapped: {
		switch(type.as.wrapped.type) {
		case wrapped_type_constant: return size_of_type(*type.as.wrapped.inner);
		case wrapped_type_pointer: return 8;
		default: todo("implement size_of_type for the type");
		}
	}
	case type_struct: {
		assert(type.as.structure.complete); // TODO: report_error("unknown structure")
		size_t max_align = 1;
		size_t offset = 0;

		for (int i=0; i<type.as.structure.field_types.len; i++) {
			struct type field_type = type.as.structure.field_types.ptr[i];
			size_t field_size = size_of_type(field_type);
			size_t field_align = alignment_of_type(field_type);

			offset = align(offset, field_size);
			offset += field_size;

			max_align = max(max_align, field_align);
		}
		return align(offset, max_align);
	}
	default: todo("implement alignment_of_type for the type");
	}
}

size_t alignment_of_type(type type) {
	switch(type.type) {
	case type_primitive: return size_of_type(type);
	case type_wrapped: {
		switch(type.as.wrapped.type) {
		case wrapped_type_pointer: return 8;
		case wrapped_type_constant: return size_of_type(*type.as.wrapped.inner);
		default: todo("implement alignment_of_type for the type");
		}
	}
	case type_struct: {
		assert(type.as.structure.complete);
		size_t max_align = 1;

		for (int i=0; i<type.as.structure.field_types.len; i++) {
			struct type field_type = type.as.structure.field_types.ptr[i];
			max_align = max(max_align, alignment_of_type(field_type));
		}
		return max_align;
	}
	default: todo("implement alignment_of_type for the type");
	}
}

size_t size_of_params(argument_list params) {
	size_t size = 0;
	for (int i=0; i<params.len; i++) {
		size += params.ptr[i].size;
	}
	return size;
}

bool type_eq(type a, type b) {
	if (a.type != b.type) return false;

	switch(a.type) {
	case type_primitive: return a.as.primitive == b.as.primitive;
	case type_wrapped: return a.as.wrapped.type == b.as.wrapped.type && 
		type_eq(*a.as.wrapped.inner, *b.as.wrapped.inner);
	case type_struct: return string_eq(a.as.structure.identifier, b.as.structure.identifier);
	case type_slice: return type_eq(*a.as.slice.inner, *b.as.slice.inner);
	case type_array: return a.as.array.size == b.as.array.size && type_eq(*a.as.array.inner, *b.as.array.inner);
	case type_function: {
		if (!type_eq(*a.as.function.return_type, *b.as.function.return_type)) return false;
		if (a.as.function.arguments.len != b.as.function.arguments.len) return false;

		for (int i=0;i<a.as.function.arguments.len;i++) {
			if (!type_eq(a.as.function.arguments.ptr[i], b.as.function.arguments.ptr[i])) return false;
		}
		return true;
	}
	case type_none: unreachable;
	}
}

typedef struct {
	type type;
	size_t offset;
	size_t size;
	size_t alignment;
} struct_field;

struct_field field_of_struct(struct_type st, string field_name) {
	size_t offset = 0;

	for (int i=0; i<st.field_types.len; i++) {
		struct type field_type = st.field_types.ptr[i];
		size_t field_size = size_of_type(field_type);

		offset = align(offset, field_size);
		if (string_eq(st.field_names.ptr[i], field_name))
			return (struct_field){field_type, offset, field_size, alignment_of_type(field_type)};
		offset += field_size;
	}
	return (struct_field){.type=type_error};
}

type type_of_expression(expression expr) {
	(void)expr;
	todo("implement type_of_expression");
}

argument compile_expression(lexer_file_loc loc, expression expr, frame* fr) {
	switch(expr.type) {
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_integer: 
			return (argument){.type=argument_literal, .value_type=argument_value_int, .size=4, .alignment=4, .value=expr.as.literal.as.integer};
		case expression_literal_char:
			return (argument){.type=argument_literal, .value_type=argument_value_int, .size=4, .alignment=4, .value=expr.as.literal.as.character};
		case expression_literal_string: {
			instruction in = {0};
			in.type = INS_LSTR;
			in.as.op.dst = frame_allocate_temp(fr, 16, 8);
			in.as.op.src1 = (argument){.type=argument_string, .offset=string_literals.len};
			array_append(&string_literals, expr.as.literal.as.string);
			array_append(&instructions, in);
			return in.as.op.dst;
		}
		};
	} break;
	case expression_type_identifier: {
		symbol* symbl = symbol_lookup(expr.as.identifier);
		if (symbl == NULL) {
			report_compiler_error(loc, tconcat(sv("unknown symbol "), expr.as.identifier));
			return argument_error;
		}

		if (symbl->symbol_type == symbol_type_local)
			return (argument){.type=argument_local, .name=expr.as.identifier, .offset=symbl->offset, .size=symbl->size, .alignment=symbl->alignment};
		else if (symbl->symbol_type == symbol_type_global) 
			return (argument){.type=argument_global, .name=expr.as.identifier, .offset=symbl->offset, .size=symbl->size, .alignment=symbl->alignment};
		else {
			report_compiler_error(loc, tconcat(symbl->identifier, sv(" is not an lvalue")));
			return argument_error;
		}
	} break;
	case expression_type_func_call: {
		expression_func_call func_call = expr.as.func_call;

		symbol* symbl = symbol_lookup(func_call.identifier);
		if (symbl == NULL) {
			report_compiler_error(loc, tconcat(sv("unknown symbol "), func_call.identifier));
			return argument_error;
		}
		type return_type = *(symbl->type.as.function.return_type);

		instruction in = {0};
		in.type = INS_CALL;
		in.label = func_call.identifier;
		in.as.call.dst = frame_allocate_temp(fr, size_of_type(return_type), alignment_of_type(return_type));

		if (func_call.expressions.len != symbl->type.as.function.arguments.len) {
			report_compiler_error(loc, tsprintf("expected %d argument(s) for function %.*s", symbl->type.as.function.arguments.len, string_arg(func_call.identifier)));
			return argument_error;
		}

		for (int i=0; i<func_call.expressions.len; i++) {
			argument param = compile_expression(loc, func_call.expressions.ptr[i], fr); 
			if (param.type == argument_none) {
				return argument_error;
			}
			
			type arg_type = symbl->type.as.function.arguments.ptr[i];
			if (arg_type.type == type_struct) {
				symbol* symbl = symbol_lookup(arg_type.as.structure.identifier);
				if (symbl == NULL) {
					report_compiler_error(loc, tconcat(sv("unknown argument type "), arg_type.as.structure.identifier));
					return argument_error;
				}
				arg_type = symbl->type;
			}

			// TODO: typechecking
			if (param.size != size_of_type(arg_type)) {
				report_compiler_error(loc, tsprintf("mismatch argument size at argument %d", i));
				return argument_error;
			}
			array_append(&in.as.call.params, param);
		}

		array_append(&instructions, in);
		return in.as.op.dst;
	} break;
	case expression_type_tree: {
		if (expr.as.tree.operands.len == 1) {
			todo("unary operator not implemented")
		} else if (expr.as.tree.operands.len == 2) {
			expression_tree tree = expr.as.tree;

			if (tree.op == operator_dot) {
				if (tree.operands.ptr[0].type != expression_type_identifier ||
					tree.operands.ptr[1].type != expression_type_identifier) {
					report_compiler_error(loc, sv("unexpected field access"));
					return argument_error;
				}

				symbol* symbl = symbol_lookup(tree.operands.ptr[0].as.identifier);
				if (symbl == NULL) {
					report_compiler_error(loc, tconcat(sv("unknown symbol "), tree.operands.ptr[0].as.identifier));
					return argument_error;
				}

				string field_name = tree.operands.ptr[1].as.identifier;
				struct_field field = field_of_struct(symbl->type.as.structure, field_name);
				if (field.type.type == type_none) {
					report_compiler_error(loc, tconcat(sv("unknown field "), field_name));
					return argument_error;
				}

				string identifier = strconcat(symbl->identifier, sv("."));
				identifier = strconcat(identifier, field_name);

				argument_type arg_type;
				if (symbl->symbol_type == symbol_type_local)
					arg_type = argument_local;
				else if (symbl->symbol_type == symbol_type_global) 
					arg_type = argument_global;
				else if (symbl->symbol_type == symbol_type_param) 
					arg_type = argument_param;
				else {
					report_compiler_error(loc, tconcat(symbl->identifier, sv(" is not an lvalue")));
					return argument_error;
				}

				return (argument){.type=arg_type, .name=identifier, .offset=symbl->offset+field.offset, .size=field.size, .alignment=field.alignment};
			} else {
				// TODO: typecheck
				instruction in = {0};

				in.as.op.src1 = compile_expression(loc, tree.operands.ptr[0], fr);
				if (in.as.op.src1.type == argument_none) return argument_error;
				
				in.as.op.dst = frame_allocate_temp(fr, in.as.op.src1.size, in.as.op.src1.alignment);

				switch(expr.as.tree.op) {
				case operator_plus:
					in.type = INS_ADD;
					break;
				case operator_minus:
					in.type = INS_SUB;
					break;
				case operator_star:
					in.type = INS_MUL;
					break;
				case operator_slash:
					in.type = INS_DIV;
					break;
				default: unreachable;
				}

				in.as.op.src2 = compile_expression(loc, tree.operands.ptr[1], fr);
				if (in.as.op.src2.type == argument_none) return argument_error;

				array_append(&instructions, in);

				return in.as.op.dst;
			}
		} else unreachable;
	} break;
	default: todo("compile_expression");;
	}
}

void compile_statement(statement stm, frame* fr) {
	switch(stm.type) {
	case statement_type_scope: {
		statement_scope sc = stm.as.scope;
		for (int i=0; i<sc.statements.len; i++) {
			compile_statement(sc.statements.ptr[i], fr);
		}
	} break;
	case statement_type_decl: {
		declaration decl = stm.as.declaration;
		if (symbol_lookup(decl.identifier) != NULL) {
			report_compiler_error(stm.loc, tconcat(sv("redefintion of symbol "), decl.identifier));
			return;
		}

		struct type decl_type;
		size_t size;
		size_t alignment;

		if (decl.type.type == type_struct) {
			symbol* struct_symbol = symbol_lookup(decl.type.as.structure.identifier);
			if (struct_symbol == NULL) {
				report_compiler_error(stm.loc, tconcat(sv("unknown identifier "), decl.type.as.structure.identifier));
				return;
			}
			decl_type = struct_symbol->type;
			size = struct_symbol->size;
			alignment = struct_symbol->alignment;
		} else {
			decl_type = decl.type;
			size = size_of_type(decl_type);
			alignment = alignment_of_type(decl_type);
		}

		argument dst = frame_allocate(argument_local, fr, decl.identifier, size, alignment);
		symbol_add(symbol_type_local, decl.identifier, decl_type, dst.offset, size, alignment);

		if (decl.value.type != expression_type_none) {
			argument src = compile_expression(stm.loc, decl.value, fr);
			if (src.type == argument_none) {
				return;
			}

			// TODO: typecheck
			if (src.size != dst.size) {
				report_compiler_error(stm.loc, sv("size mismatch in assignment"));
				return;
			}

			// TODO: sizeof(word)
			if (src.size <= 8) {
				instruction in = (instruction){0};
				in.type = INS_STORE;
				in.as.op.dst = dst;
				in.as.op.src1 = src;
				array_append(&instructions, in);
			} else {
				argument t0 = frame_allocate_temp(fr, 8, 8);
				argument t1 = frame_allocate_temp(fr, 8, 8);

				instruction in = (instruction){0};
				in.type = INS_LEA;
				in.as.op.dst = t0;
				in.as.op.src1 = dst;
				array_append(&instructions, in);

				in = (instruction){0};
				in.type = INS_LEA;
				in.as.op.dst = t1;
				in.as.op.src1 = src;
				array_append(&instructions, in);

				in = (instruction){0};
				in.type = INS_COPY;
				in.as.op.dst = t0;
				in.as.op.src1 = t1;
				in.as.op.src2 = (argument){.type=argument_literal, .value_type=argument_value_int, .value=src.size};
				array_append(&instructions, in);
			}
		}
	} break;
	case statement_type_assign: {
		statement_assign assign = stm.as.assignment;
		symbol* symbl = symbol_lookup(assign.destination.identifier);
		if (symbl == NULL) {
			report_compiler_error(stm.loc, tconcat(sv("unknown identifier "), assign.destination.identifier));
			return;
		}

		argument_type arg_type;
		if (symbl->symbol_type == symbol_type_local)
			arg_type = argument_local;
		else if (symbl->symbol_type == symbol_type_global) 
			arg_type = argument_global;
		else if (symbl->symbol_type == symbol_type_param) 
			arg_type = argument_param;
		else {
			report_compiler_error(stm.loc, tconcat(assign.destination.identifier, sv("is not an lvalue")));
			return;
		}

		argument dst = (argument){.type=arg_type, .name=assign.destination.identifier, .offset=symbl->offset, .size=symbl->size, .alignment=symbl->alignment};
		argument src = compile_expression(stm.loc, assign.value, fr);
		if (src.type == argument_none) {
			return;
		}

		// TODO: typecheck
		if (src.size != dst.size) {
			report_compiler_error(stm.loc, sv("size mismatch in assignment"));
			return;
		}

		instruction in = (instruction){0};
		in.type = (src.size > 8)?INS_COPY:INS_STORE;
		in.as.op.dst = dst;
		in.as.op.src1 = src;
		array_append(&instructions, in);
	} break;
	case statement_type_return: {
		argument arg = compile_expression(stm.loc, stm.as.ret.value, fr);
		if (arg.type == argument_none) {
			return;
		}
		if (arg.size > 4) {
			report_compiler_error(stm.loc, sv("expression must be integer"));
			return;
		}
		instruction in = (instruction){.type = INS_RET, .as = {.op = {.dst=arg}}};
		array_append(&instructions, in);
	} break;
	default: todo("implement compile_statement");
	}
}

argument_list compile_function_params(function fn) {
	argument_list params = {0};

	size_t offset = 0;
	for (int i=0; i<fn.arguments.len; i++) {
		binding bind = fn.arguments.ptr[i];
		if (symbol_lookup(bind.identifier) != NULL) {
			report_compiler_error(fn.loc, tconcat(sv("redefintion of symbol "), bind.identifier));
			return params;
		}

		struct type decl_type;
		size_t size;
		size_t alignment;

		if (bind.type.type == type_struct) {
			symbol* struct_symbol = symbol_lookup(bind.type.as.structure.identifier);
			if (struct_symbol == NULL) {
				report_compiler_error(fn.loc, tconcat(sv("unknown identifier "), bind.type.as.structure.identifier));
				return params;
			}
			decl_type = struct_symbol->type;
			size = struct_symbol->size;
			alignment = struct_symbol->alignment;
		} else {
			decl_type = bind.type;
			size = size_of_type(decl_type);
			alignment = alignment_of_type(decl_type);
		}

		argument_value_type arg_value_type = (size <= 8)? 
			argument_value_int: argument_value_struct;
		
		offset = align(offset, size);
		offset += size;
		symbol_add(symbol_type_param, bind.identifier, decl_type, offset, size, alignment);

		argument arg = (argument){.type=argument_param, .value_type=arg_value_type, .name=bind.identifier, .offset=offset, .size=size, .alignment=alignment};	
		array_append(&params, arg);
	}

	return params;
}

void compile_function(function fn) {
	symbol* symbl = symbol_lookup(fn.identifier);
	if (symbl != NULL) {
		report_compiler_error(fn.loc, tconcat(sv("redefinition of symbl "), fn.identifier));
		return;
	}
	symbol_add(symbol_type_function, fn.identifier, type_of_function(fn), 0, 8, 1);

	symbol_table saved = symbols;
	argument_list params = compile_function_params(fn);

	frame* fr = calloc(1, sizeof(frame));

	instruction in = (instruction){.type = INS_FUNC_START, .label=fn.identifier, .as = {.func = {.params=params, .frame=fr}}};
	array_append(&instructions, in);

	for (int i=0; i<fn.body.len; i++) {
		compile_statement(fn.body.ptr[i], fr);
	}
	symbols.first = saved.first;
	symbols.last = saved.last;

	in = (instruction){.type = INS_FUNC_END, .label=fn.identifier};
	array_append(&instructions, in);
}

void compile_structure(structure strukt) {
	symbol* symbl = symbol_lookup(strukt.identifier);
	if (symbl != NULL) {
		report_compiler_error(strukt.loc, tconcat(sv("redefinition of symbl "), strukt.identifier));
		return;
	}

	type strukt_type = type_of_struct(strukt);
	size_t size = size_of_type(strukt_type);
	size_t alignment = alignment_of_type(strukt_type);

	symbol_add(symbol_type_struct, strukt.identifier, strukt_type, 0, size, alignment);
}

intermediate_representation compile(program prg) {
	intermediate_representation ir = {0};

	for (int i=0; i<prg.structs.len; i++) {
		compile_structure(prg.structs.ptr[i]);
	}

	for (int i=0; i<prg.functions.len; i++) {
		compile_function(prg.functions.ptr[i]);
	}

	ir.instructions = instructions;
	ir.strings = string_literals;
	print_ir(ir);
	return ir;
}