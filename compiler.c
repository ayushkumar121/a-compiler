typedef enum {
	ins_label,
	ins_binop,
	ins_func_start,
	ins_func_end,
	ins_func_call,
	ins_ret,
} instruction_type;

typedef enum {
	op_add,
	op_sub,
	op_mul,
	op_div,
	// stores dst+src1*src2 to dst
	op_madd,
	// loads of the pointer of src1 to dst
	op_addrof,
	// stores the values of src1 to dst
	op_store,
	// loads the value passed in param to dst
	op_load_param,
	// loads the value stored in pointer stored in src1 to dst, loads src2 bytes
	op_load_indirect,
	// stores the values of src1 to pointer in dst, stores src2 bytes
	op_store_indirect,
	// copies src2 bytes from src1 to dst
	op_copy,
} op_type;

typedef enum {
	argument_type_none,
	argument_type_local,
	argument_type_global,
	argument_type_param,
	argument_type_string,
	argument_type_literal,
} argument_type;

typedef struct {
	argument_type type;
	int size;
	union {
		uint64_t value;
		int index;
		int offset;
	} as;
} argument;

typedef struct {
	string identifier;
	int size;
	int count;
} frame;

typedef struct {
	instruction_type type;
	lexer_file_loc loc;
	union {
		string label;
		frame* frame;
		argument ret;
		struct {op_type type; argument dst, src1, src2;} op;
		struct {argument dst; string identifier; int argc; argument* args;} fcall;
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
	symbol_type_local,
	symbol_type_global,
	symbol_type_function,
	symbol_type_struct,
} symbol_type;

typedef struct symbol {
	symbol_type symbol_type;
	string identifier;
	type* type; 
	int offset;
	symbol* next;
} symbol;

#define PTR_SIZE 8

symbol* symbol_top = NULL;
symbol* symbol_saved = NULL;
strings string_literals = {0};
instruction_list instructions = {0};
lexer_file_loc loc;

void symbol_add(symbol_type symbol_type, string identifier, type type, int offset) {
	symbol* new_symbol = malloc(sizeof(symbol));
	new_symbol->symbol_type = symbol_type;
	new_symbol->identifier = identifier;
	new_symbol->type = malloc(sizeof(type));
	memcpy(new_symbol->type, &type, sizeof(type));
	new_symbol->offset = offset;
	new_symbol->next = symbol_top;

	symbol_top = new_symbol;
}

void symbol_dump() {
	symbol* iter = symbol_top;
	printf("----SYMBOLS----\n");
	while(iter != NULL) {
		printf(sfmt":"sfmt"\n", sarg(iter->identifier), sarg(type_to_string(*iter->type)));
		iter = iter->next;
	}
	printf("----SYMBOLS----\n");
}

symbol* symbol_lookup(string identifier, symbol_type symbol_type) {
	symbol* iter = symbol_top;
	while(iter != NULL) {
		if (string_eq(iter->identifier, identifier) && iter->symbol_type == symbol_type) return iter;
		iter = iter->next;
	}
	return NULL;
}

type resolve_complete_type(type type) {
	switch (type.type) {
	case type_primitive: return type;
	case type_wrapped: {
		*type.as.wrapped.inner = resolve_complete_type(*type.as.wrapped.inner);
		return type;
	} 
	case type_array: {
		*type.as.array.inner = resolve_complete_type(*type.as.array.inner);
		return type;
	}
	case type_slice: {
		*type.as.slice.inner = resolve_complete_type(*type.as.slice.inner);
		return type;
	}
	case type_struct: {
		if (type.as.structure.complete) return type;

		symbol* symbl = symbol_lookup(type.as.structure.identifier, symbol_type_struct);
		if (!symbl) return type_error;
		return *symbl->type;
	}
	case type_function: {
		symbol* symbl = symbol_lookup(type.as.function.identifier, symbol_type_function);
		if (!symbl) return type_error;
		return *symbl->type;
	}
	case type_none: unreachable;
	}
}

int alignment_of_type(type* type);

int size_of_type(type* type) {
	switch(type->type) {
	case type_primitive: {
		switch(type->as.primitive) {
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
		case primitive_string: return 2*PTR_SIZE;
		case primitive_none: unreachable;
		}
	}
	case type_wrapped: {
		switch(type->as.wrapped.type) {
		case wrapped_type_constant: return size_of_type(type->as.wrapped.inner);
		case wrapped_type_pointer: return PTR_SIZE;
		default: todo("implement size_of_type for the type");
		}
	}
	case type_struct: {
		ASSERT(type->as.structure.complete);

		size_t max_align = 1;
		size_t offset = 0;

		for (int i=0; i<type->as.structure.field_count; i++) {
			struct type field_type = type->as.structure.field_types[i];
			size_t field_size = size_of_type(&field_type);
			size_t field_align = alignment_of_type(&field_type);

			offset = align(offset, field_size);
			offset += field_size;

			max_align = max(max_align, field_align);
		}
		return align(offset, max_align);
	}
	case type_array: return type->as.array.size*size_of_type(type->as.array.inner);
	default: todo("implement size_of_type for the type");
	}
}

int alignment_of_type(type* type) {
	switch(type->type) {
	case type_primitive: return size_of_type(type);
	case type_wrapped:return size_of_type(type);
	case type_struct: {
		ASSERT(type->as.structure.complete);

		int max_align = 1;
		for (int i=0; i<type->as.structure.field_count; i++) {
			struct type field_type = type->as.structure.field_types[i];
			max_align = max(max_align, alignment_of_type(&field_type));
		}
		return max_align;
	}
	case type_array: return alignment_of_type(type->as.array.inner);
	default: todo("implement alignment_of_type for the type");
	}
}

inline static void add_instruction_label(string label) {
	array_append(&instructions, ((instruction){.type=ins_label, .as = {.label=label}}));
}

inline static void add_instruction_func_start(frame* frame) {
	array_append(&instructions, ((instruction){.type=ins_func_start, .as = {.frame=frame}}));
}

inline static void add_instruction_func_end(frame* frame) {
	array_append(&instructions, ((instruction){.type=ins_func_end, .as = {.frame=frame}}));
}

inline static void add_instruction_func_call(argument dst, string identifier, int argc, argument* args) {
	array_append(&instructions, ((instruction){.type=ins_func_call, .as = {.fcall={.dst=dst, .identifier=identifier, .argc=argc, .args=args}}}));
}

inline static void add_instruction_op(op_type optype, argument dst, argument src1) {
	array_append(&instructions, ((instruction){.type=ins_binop, .as = {.op={.type=optype, .dst=dst, .src1=src1}}}));
}

inline static void add_instruction_op2(op_type optype, argument dst, argument src1, argument src2) {
	array_append(&instructions, ((instruction){.type=ins_binop, .as = {.op={.type=optype, .dst=dst, .src1=src1, .src2=src2}}}));
}

inline static void add_instruction_ret(argument value) {
	array_append(&instructions, ((instruction){.type=ins_ret, .as = {.ret=value}}));
}

inline static argument argument_none() {
	return (argument){.type=argument_type_none};
}

inline static argument argument_string_literal(string str) {
	argument arg = (argument){.type=argument_type_string, .size=PTR_SIZE, .as={.index=string_literals.len}};
	array_append(&string_literals, str);
	return arg;
}

inline static argument argument_literal(int value) {
	return (argument){.type=argument_type_literal, .size=PTR_SIZE, .as={.value=value}};
}

inline static argument argument_local(int offset, int size) {
	return (argument){.type=argument_type_local, .size=size, .as={.offset=offset}};
}

inline static argument argument_global(int offset, int size) {
	return (argument){.type=argument_type_global, .size=size, .as={.offset=offset}};
}

inline static argument argument_param(int index, int size) {
	return (argument){.type=argument_type_param, .size=size, .as={.index=index}};
}

argument argument_allocate(frame* frame, int size) {
    size_t alignment = max(size, 8);
    frame->size = align(frame->size, alignment);
    int offset = frame->size;

    frame->size += alignment;
    frame->count++;

	return (argument){.type=argument_type_local, .size=size, .as={.offset=offset}};
}

argument argument_from_symbol(symbol* symbl) {
	ASSERT(symbl != NULL);
	size_t size = size_of_type(symbl->type);
	if (symbl->symbol_type == symbol_type_local)
	return (argument){.type=argument_type_local, .size=size, .as={.offset=symbl->offset}};
	else if (symbl->symbol_type == symbol_type_global)
	return (argument){.type=argument_type_global, .size=size, .as={.offset=symbl->offset}};
	else unreachable;
}

argument argument_field(argument base, int offset, int size) {
	ASSERT(base.type == argument_type_local || base.type == argument_type_global);
	base.as.offset += offset;
	base.size = size;
	return base;
}

typedef struct {
	int offset;
	int size;
	type type;
} field;

field struct_field(struct_type stype, string fieldname) {
	ASSERT(stype.complete);
	size_t offset = 0;
	for (int i=0; i<stype.field_count; i++) {
		type field_type = stype.field_types[i];
		int field_size = size_of_type(&field_type);
		offset = align(offset, field_size);
		if (string_eq(stype.field_names[i], fieldname)) return (field){offset, field_size, field_type};
		offset += field_size;
	}
	return (field){-1, -1, {}};
}

bool type_eq(type* a, type* b) {
	if (a->type != b->type) return false;
	switch(a->type) {
	case type_primitive: return a->as.primitive == b->as.primitive;
	case type_wrapped: return a->as.wrapped.type == b->as.wrapped.type && 
		type_eq(a->as.wrapped.inner, b->as.wrapped.inner);
	case type_struct: return string_eq(a->as.structure.identifier, b->as.structure.identifier);
	case type_slice: return type_eq(a->as.slice.inner, b->as.slice.inner);
	case type_array: return a->as.array.size == b->as.array.size && type_eq(a->as.array.inner, b->as.array.inner);
	case type_function: return string_eq(a->as.function.identifier, b->as.function.identifier);
	case type_none: unreachable;
	}
}

type type_of_expression(expression expr) {
	switch(expr.type) {
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_integer: return type_of_primitive(primitive_int);
		case expression_literal_char: return type_of_primitive(primitive_int);
		case expression_literal_string: return type_of_primitive(primitive_string);
		}
	}
	case expression_type_identifier: {
		symbol* symbl = symbol_lookup(expr.as.identifier, symbol_type_local);
		if (!symbl) symbl = symbol_lookup(expr.as.identifier, symbol_type_global);
		ASSERT(symbl && symbl->type);
		return *symbl->type;
	}
	case expression_type_tree: {
		if (expr.as.tree.operands.len == 1) {
			switch(expr.as.tree.op) {
			case operator_ampersand: {
				type tmp = type_of_expression(expr.as.tree.operands.ptr[0]);
				type* inner = malloc(sizeof(type));
				memcpy(inner, &tmp, sizeof(type));
				return type_of_wrapped(wrapped_type_pointer, inner);
			}
			default: unreachable;
			}
		} else if (expr.as.tree.operands.len == 2) {
			switch(expr.as.tree.op) {
			case operator_plus:
			case operator_minus:
			case operator_star:
			case operator_slash:
				return type_of_expression(expr.as.tree.operands.ptr[0]);
			case operator_index:
				return type_of_expression(expr.as.tree.operands.ptr[0]);
			default: unreachable;
			}
		} else unreachable;
	}
	default: unreachable;
	}
}

string type_mismatch_error(type exptected, type found) {
	return tsprintf("exptected type ("sfmt") found ("sfmt")", 
		sarg(type_to_string(exptected)), sarg(type_to_string(found)));
}

argument compile_expression(frame* frame, expression expr);

argument compile_binop(frame* frame, expression_tree expr_tree) {
	ASSERT(expr_tree.operands.len == 2);

	if (expr_tree.op == operator_index) {
		expression expr = expr_tree.operands.ptr[0];
		if (expr.type != expression_type_identifier) {
			report_compiler_error(loc, sv("cannot index into expression"));
			return argument_none();
		}

		string identifier = expr.as.identifier;
		symbol* symbl = symbol_lookup(identifier, symbol_type_local);
		if (!symbl) symbol_lookup(identifier, symbol_type_global);
		if (!symbl)  {
			report_compiler_error(loc, tconcat(sv("unknown symbol "), identifier));
			return argument_none();
		}
		type* decl_type = symbl->type;

		if (decl_type->type == type_array) {
			int elemsize = size_of_type(decl_type->as.array.inner);
			argument base = argument_from_symbol(symbl);
			argument index = compile_expression(frame, expr_tree.operands.ptr[1]);
			if (!index.type) return argument_none();

			argument dst = argument_allocate(frame, elemsize);
			argument t0 = argument_allocate(frame, PTR_SIZE);
			add_instruction_op(op_addrof, t0, base);
			add_instruction_op2(op_madd, t0, index, argument_literal(elemsize));
			add_instruction_op2(op_load_indirect, dst, t0, argument_literal(elemsize));
			return dst;
		} else if (decl_type->type == type_primitive && decl_type->as.primitive == primitive_string) {
			// string = {size_t, char*}
			argument data = argument_field(argument_from_symbol(symbl), PTR_SIZE, PTR_SIZE);
			argument index = compile_expression(frame, expr_tree.operands.ptr[1]);
			if (!index.type) return argument_none();

			argument t0 = argument_allocate(frame, PTR_SIZE);
			add_instruction_op(op_store, t0, data);
			add_instruction_op2(op_add, t0, t0, index);
				
			argument dst = argument_allocate(frame, 1);
			add_instruction_op2(op_load_indirect, dst, t0, argument_literal(1));

			return dst;
		} else {
			report_compiler_error(loc, sv("cannot index into expression"));
			return argument_none();
		}
	} else if (expr_tree.op == operator_dot) {
		expression expr = expr_tree.operands.ptr[0];
		if (expr.type != expression_type_identifier) {
			report_compiler_error(loc, sv("cannot access field of expression"));
			return argument_none();
		}

		expression fexpr = expr_tree.operands.ptr[1];
		if (fexpr.type != expression_type_identifier) {
			report_compiler_error(loc, sv("not a valid field name"));
			return argument_none();
		}

		string identifier = expr.as.identifier;
		string fieldname = fexpr.as.identifier;

		symbol* symbl = symbol_lookup(identifier, symbol_type_local);
		if (!symbl) symbol_lookup(identifier, symbol_type_global);
		if (!symbl)  {
			report_compiler_error(loc, tconcat(sv("unknown symbol "), identifier));
			return argument_none();
		}
		type* decl_type = symbl->type;

		if (decl_type->type != type_struct) {
			report_compiler_error(loc, tconcat(identifier, sv(" is not a valid struct")));
			return argument_none();
		}

		field f = struct_field(decl_type->as.structure, fieldname);
		if (f.offset == -1) {
			report_compiler_error(loc, tconcat(sv("unknown field "), fieldname));
			return argument_none();
		}

		argument base = argument_from_symbol(symbl);
		argument dst = argument_allocate(frame, f.size);
		argument t0 = argument_allocate(frame, PTR_SIZE);
		add_instruction_op(op_addrof, t0, base);
		add_instruction_op2(op_add, t0, t0, argument_literal(f.offset));
		add_instruction_op2(op_load_indirect, dst, t0, argument_literal(f.size));
		return dst;
	} else {
		op_type op;
		switch(expr_tree.op) {
		case operator_plus: op = op_add; break;
		case operator_minus: op = op_sub; break;
		case operator_star: op = op_mul; break;
		case operator_slash: op = op_div; break;
		default: unreachable;
		}

		argument src1 = compile_expression(frame, expr_tree.operands.ptr[0]);
		if (!src1.type) return argument_none();
		argument src2 = compile_expression(frame, expr_tree.operands.ptr[1]);
		if (!src2.type) return argument_none();

		argument dst = argument_allocate(frame, src1.size);
		add_instruction_op2(op, dst, src1, src2);
		return dst;
	}
}

argument compile_uniop(frame* frame, expression_tree expr_tree) {
	ASSERT(expr_tree.operands.len == 1);

	if (expr_tree.op == operator_ampersand) {
		argument dst = argument_allocate(frame, PTR_SIZE);
		argument src = compile_expression(frame, expr_tree.operands.ptr[0]);
		if (src.type == argument_type_none) return argument_none();
		add_instruction_op(op_addrof, dst, src);
		return dst;
	} else unreachable;
}

argument compile_fcall(frame* frame, func_call fcall) {
	symbol* symbl = symbol_lookup(fcall.identifier, symbol_type_function);
	if (!symbl) {
		report_compiler_error(loc, tconcat(sv("unknown function "), fcall.identifier));
		return argument_none();
	}

	type* return_type = symbl->type->as.function.return_type;
	argument dst = argument_allocate(frame, size_of_type(return_type));

	int argc = fcall.expressions.len;
	argument* args = calloc(argc, sizeof(argument));

	for (int i=0; i<fcall.expressions.len; i++) {
		argument arg = compile_expression(frame, fcall.expressions.ptr[i]);
		if (arg.type == argument_type_none) {
			return argument_none();
		}
		args[i] = arg;
	}

	add_instruction_func_call(dst, fcall.identifier, argc, args);
	return dst;
}

argument compile_expression(frame* frame, expression expr) {
	// printf("compile_expression "sfmt" \n", sarg(expression_to_string(expr)));

	switch(expr.type) {
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_integer: return argument_literal(expr.as.literal.as.integer);
		case expression_literal_char: return argument_literal(expr.as.literal.as.character);
		case expression_literal_string: {
			argument base = argument_allocate(frame, 2*PTR_SIZE);
			add_instruction_op(op_store, argument_field(base, 0, PTR_SIZE), argument_literal(expr.as.literal.as.string.len));
			add_instruction_op(op_store, argument_field(base, PTR_SIZE, PTR_SIZE), argument_string_literal(expr.as.literal.as.string));
			return base;
		}
		}
	}
	case expression_type_identifier: {
		symbol* symbl = symbol_lookup(expr.as.identifier, symbol_type_local);
		if (!symbl) symbl = symbol_lookup(expr.as.identifier, symbol_type_global);
		if (!symbl) {
			report_compiler_error(loc, tconcat(sv("unknown identifier "), expr.as.identifier));
			return argument_none();
		}
		return argument_from_symbol(symbl);
	}
    case expression_type_func_call: {
    	return compile_fcall(frame, expr.as.func_call);
    }
	case expression_type_tree: {
		if (expr.as.tree.operands.len == 1) {
			return compile_uniop(frame, expr.as.tree);
		} else if (expr.as.tree.operands.len == 2) {
			return compile_binop(frame, expr.as.tree);
		} else unreachable;
	}
	case expression_type_none: return argument_none();
	}
}

void compile_statement(frame* frame, statement stm);

void compile_statement_list(frame* frame, statement_list stms) {
	for (int i=0; i<stms.len; i++) {
		compile_statement(frame, stms.ptr[i]);
	}
}

void compile_statement(frame* frame, statement stm) {
	loc = stm.loc;

	switch(stm.type) {
	case statement_type_list: {
		symbol* saved = symbol_top;
		compile_statement_list(frame, stm.as.list);
		symbol_top = saved;
	} break;

	case statement_type_decl: {
		declaration decl = stm.as.declaration;
		type decl_type = resolve_complete_type(decl.type);
		if (decl_type.type == type_none) {
			report_compiler_error(loc, tconcat(sv("unknown type "), type_to_string(decl_type)));
			return;
		}

		argument dst = argument_allocate(frame, size_of_type(&decl_type));
		symbol_add(symbol_type_local, decl.identifier, decl_type, dst.as.offset);

		if (decl.value.type != expression_type_none) {
			argument src = compile_expression(frame, decl.value);
			if (src.type == argument_type_none) return;

			type value_type = type_of_expression(decl.value);
			if (!type_eq(&decl_type, &value_type)) {
				report_compiler_error(loc, type_mismatch_error(decl_type, value_type));
				return;
			}

			if (dst.size <= PTR_SIZE)
				add_instruction_op(op_store, dst, src);
			else 
				add_instruction_op2(op_copy, dst, src, argument_literal(dst.size));
		}
	} break;

	case statement_type_assign: {
		statement_assign assignment = stm.as.assignment;

		symbol* symbl = symbol_lookup(assignment.lvalue.identifier, symbol_type_local);
		if (!symbl) symbl = symbol_lookup(assignment.lvalue.identifier, symbol_type_global);
		if (!symbl) {
			report_compiler_error(loc, tconcat(sv("unknown identifier "), assignment.lvalue.identifier));
			return;
		}
		type* decl_type = symbl->type;

		switch(assignment.lvalue.type) {
		case lvalue_type_variable: {
			argument dst = argument_from_symbol(symbl);
			argument src = compile_expression(frame, assignment.value);
			if (src.type == argument_type_none) return;

			type value_type = type_of_expression(assignment.value);
			if (!type_eq(decl_type, &value_type)) {
				report_compiler_error(loc, type_mismatch_error(*decl_type, value_type));
				return;
			}

			if (dst.size <= PTR_SIZE)
				add_instruction_op(op_store, dst, src);
			else 
				add_instruction_op2(op_copy, dst, src, argument_literal(dst.size));
		} break;

		case lvalue_type_indexed: {
			if (decl_type->type != type_array) {
				report_compiler_error(loc, tconcat(sv("cannot index into "), assignment.lvalue.identifier));
				return;
			}

			int elemsize = size_of_type(decl_type->as.array.inner);
			argument base = argument_from_symbol(symbl);
			argument index = compile_expression(frame, assignment.lvalue.as.index);
			if (index.type == argument_type_none) return;
			argument value = compile_expression(frame, assignment.value);
			if (value.type == argument_type_none) return;

			argument t0 = argument_allocate(frame, PTR_SIZE);
			add_instruction_op(op_addrof, t0, base);
			add_instruction_op2(op_madd, t0, index, argument_literal(elemsize));
			add_instruction_op2(op_store_indirect, t0, value, argument_literal(elemsize));
		} break;

		case lvalue_type_field: {
			if (decl_type->type != type_struct) {
				report_compiler_error(loc, tconcat(sv("cannot access field of "), assignment.lvalue.identifier));
				return;
			}
			field f = struct_field(decl_type->as.structure, assignment.lvalue.as.field);
			if (f.offset == -1) {
				report_compiler_error(loc, tconcat(sv("unknown field "), assignment.lvalue.as.field));
				return;
			}

			argument base = argument_from_symbol(symbl);
			argument value = compile_expression(frame, assignment.value);
			if (value.type == argument_type_none) return;

			argument t0 = argument_allocate(frame, PTR_SIZE);
			add_instruction_op(op_addrof, t0, base);
			add_instruction_op2(op_add, t0, t0, argument_literal(f.offset));
			add_instruction_op2(op_store_indirect, t0, value, argument_literal(f.size));
		} break;

		default: unreachable;
		}
	} break;

	case statement_type_return: {
		argument value;

		if (stm.as.ret.value.type != expression_type_none) {
			value = compile_expression(frame, stm.as.ret.value);
			if (value.type == argument_type_none) return;
		} else {
			value = argument_none();
		}
		add_instruction_ret(value);
		return;
	} break;

	default: unreachable;
	}
}

void compile_structure(structure strukt) {
	loc = strukt.loc;
	if (symbol_lookup(strukt.identifier, symbol_type_struct)) {
		report_compiler_error(strukt.loc, tconcat(sv("redefinition of structure "), strukt.identifier));
		return;
	}
	symbol_add(symbol_type_struct, strukt.identifier, type_of_struct(strukt), 0);
}

void compile_function(function func) {
	fprintf(stderr, "info: compiling func "sfmt"\n", sarg(func.identifier));

	loc = func.loc;
	if (symbol_lookup(func.identifier, symbol_type_function)) {
		report_compiler_error(loc, tconcat(sv("redefinition of function "), func.identifier));
		return;
	}
	symbol_add(symbol_type_function, func.identifier, type_of_function(func), 0);

	add_instruction_label(func.identifier);
	frame* frame = calloc(1, sizeof(*frame));
	frame->identifier = func.identifier;
	add_instruction_func_start(frame);

	symbol* saved = symbol_top;
	for (int i=0; i<func.arguments.len; i++) {
		binding bind = func.arguments.ptr[i];
		type decl_type = resolve_complete_type(bind.type);
		if (decl_type.type == type_none) {
			report_compiler_error(loc, tconcat(sv("unknown type "), type_to_string(decl_type)));
			return;
		}

		int size = size_of_type(&decl_type);
		argument dst = argument_allocate(frame, size);
		add_instruction_op(op_load_param, dst, argument_param(i, size));

		symbol_add(symbol_type_local, bind.identifier, decl_type, dst.as.offset);
	}
	compile_statement_list(frame, func.body);
	symbol_top = saved;

	add_instruction_func_end(frame);
}

intermediate_representation compile(program prg) {
	fprintf(stderr, "info: compiling program\n");

	for (int i=0; i<prg.structs.len; i++) {
		compile_structure(prg.structs.ptr[i]);
	}

	for (int i=0; i<prg.functions.len; i++) {
		compile_function(prg.functions.ptr[i]);
	}
	return (intermediate_representation){string_literals, instructions};
}

// string argument_to_string(argument arg) {
// 	switch(arg.type) {
// 	case argument_none: 
// 		return tsprintf("<empty>");
// 	case argument_string: 
// 		return tsprintf("string(index=%zu)", arg.offset);
// 	case argument_literal: 
// 		return tsprintf("literal(value_type=%.*s size=%zu alignment=%zu value=%zu)", 
// 			string_arg(argument_value_to_string(arg.value_type)), arg.size, arg.alignment, arg.value);
// 	case argument_param: 
// 		return tsprintf("param(name=%.*s value_type=%.*s index=%zu size=%zu alignment=%zu)", 
// 			string_arg(arg.name), string_arg(argument_value_to_string(arg.value_type)), arg.offset, arg.size, arg.alignment); break;
// 	case argument_local: 
// 		return tsprintf("local(name=%.*s value_type=%.*s offset=%zu size=%zu alignment=%zu)", 
// 			string_arg(arg.name),  string_arg(argument_value_to_string(arg.value_type)), arg.offset, arg.size, arg.alignment); break;
// 	case argument_global:
// 		return tsprintf("global(name=%.*s value_type=%.*s offset=%zu size=%zu alignment=%zu)", 
// 			string_arg(arg.name), string_arg(argument_value_to_string(arg.value_type)), arg.offset, arg.size, arg.alignment); break;
// 	}
// }

// argument compile_expression(lexer_file_loc loc, expression expr, frame* fr) {
// 	switch(expr.type) {
// 	case expression_type_literal: {
// 		switch(expr.as.literal.type) {
// 		case expression_literal_integer: 
// 			return (argument){.type=argument_literal, .size=4, .alignment=4, .value=expr.as.literal.as.integer};
// 		case expression_literal_char:
// 			return (argument){.type=argument_literal, .size=4, .alignment=4, .value=expr.as.literal.as.character};
// 		case expression_literal_string: {
// 			string str = expr.as.literal.as.string;
// 			argument dst = frame_allocate_temp(fr, argument_value_aggregate, 16, 8);

// 			instruction in = {0};
// 			in.type = INS_STORE;
// 			in.as.op.dst = argument_field(dst, 0, 8);
// 			in.as.op.src1 = (argument){.type=argument_literal, .size=8, .alignment=8, .value=str.len};
// 			array_append(&instructions, in);

// 			in = (instruction){0};
// 			in.type = INS_STORE;
// 			in.as.op.dst = argument_field(dst, 8, 0);
// 			in.as.op.src1 = (argument){.type=argument_string, .size=8, .alignment=8, .value=string_literals.len};
// 			array_append(&instructions, in);

// 			array_append(&string_literals, str);
// 			return dst;
// 		}
// 		};
// 	} break;
// 	case expression_type_identifier: {
// 		symbol* symbl = symbol_lookup(expr.as.identifier);
// 		if (symbl == NULL) {
// 			report_compiler_error(loc, tconcat(sv("unknown symbol "), expr.as.identifier));
// 			return argument_error;
// 		}

// 		return symbol_to_argument(loc, symbl);
// 	} break;
// 	case expression_type_func_call: {
// 		func_call func_call = expr.as.func_call;

// 		symbol* symbl = symbol_lookup(func_call.identifier);
// 		if (symbl == NULL) {
// 			report_compiler_error(loc, tconcat(sv("unknown symbol "), func_call.identifier));
// 			return argument_error;
// 		}
// 		type return_type = *(symbl->type.as.function.return_type);

// 		instruction in = {0};
// 		in.type = INS_CALL;
// 		in.label = func_call.identifier;

// 		size_t size = size_of_type(return_type);
// 		size_t alignment = alignment_of_type(return_type);
// 		argument_value_type value_type = (size<=8)?argument_value_int:argument_value_aggregate;
// 		in.as.call.dst = frame_allocate_temp(fr, value_type, size, alignment);

// 		if (func_call.expressions.len != symbl->type.as.function.arguments.len) {
// 			report_compiler_error(loc, tsprintf("expected %d argument(s) for function %.*s", symbl->type.as.function.arguments.len, string_arg(func_call.identifier)));
// 			return argument_error;
// 		}

// 		for (int i=0; i<func_call.expressions.len; i++) {
// 			argument param = compile_expression(loc, func_call.expressions.ptr[i], fr); 
// 			if (param.type == argument_none) {
// 				return argument_error;
// 			}
			
// 			type arg_type = symbl->type.as.function.arguments.ptr[i];
// 			if (arg_type.type == type_struct) {
// 				symbol* symbl = symbol_lookup(arg_type.as.structure.identifier);
// 				if (symbl == NULL) {
// 					report_compiler_error(loc, tconcat(sv("unknown argument type "), arg_type.as.structure.identifier));
// 					return argument_error;
// 				}
// 				arg_type = symbl->type;
// 			}

// 			// TODO: typechecking
// 			if (param.size != size_of_type(arg_type)) {
// 				report_compiler_error(loc, tsprintf("mismatch argument size at argument %d", i));
// 				return argument_error;
// 			}
// 			array_append(&in.as.call.params, param);
// 		}

// 		array_append(&instructions, in);
// 		return in.as.op.dst;
// 	} break;
// 	case expression_type_tree: {
// 		if (expr.as.tree.operands.len == 1) {
// 			todo("unary operator not implemented")
// 		} else if (expr.as.tree.operands.len == 2) {
// 			expression_tree tree = expr.as.tree;

// 			if (tree.op == operator_dot) {
// 				if (tree.operands.ptr[0].type != expression_type_identifier ||
// 					tree.operands.ptr[1].type != expression_type_identifier) {
// 					report_compiler_error(loc, sv("unexpected field access"));
// 					return argument_error;
// 				}

// 				symbol* symbl = symbol_lookup(tree.operands.ptr[0].as.identifier);
// 				if (symbl == NULL) {
// 					report_compiler_error(loc, tconcat(sv("unknown symbol "), tree.operands.ptr[0].as.identifier));
// 					return argument_error;
// 				}

// 				if (symbl->type.type != type_struct) {
// 					report_compiler_error(loc, tconcat(sv("unknown struct "), symbl->identifier));
// 					return argument_error;
// 				}

// 				string field_name = tree.operands.ptr[1].as.identifier;
// 				struct_field field = field_of_struct(symbl->type.as.structure, field_name);
// 				if (field.type.type == type_none) {
// 					report_compiler_error(loc, tconcat(sv("unknown field "), field_name));
// 					return argument_error;
// 				}

// 				argument base = symbol_to_argument(loc, symbl);
// 				if (base.type == argument_none) {
// 					return argument_error;
// 				}

// 				argument t0 = frame_allocate_temp(fr, argument_value_ref, 8, 8);  // TODO: size of pointer
// 				argument_value_type value_type = (field.size<=8)?argument_value_int:argument_value_aggregate;
// 				argument t1 = frame_allocate_temp(fr, value_type, field.size, field.alignment);

// 				instruction in = {0};
// 				in.type = INS_LEA;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = base;
// 				array_append(&instructions, in);

// 				in = (instruction){0};
// 				in.type = INS_ADD;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = t0;
// 				in.as.op.src2 = CLITERAL(field.offset);
// 				array_append(&instructions, in);

// 				if (field.size <= 8) {
// 					in = (instruction){0};
// 					in.type = INS_LOAD;
// 					in.as.op.dst = t1;
// 					in.as.op.src1 = t0;
// 					array_append(&instructions, in);
// 				} else {				
// 					argument t2 = frame_allocate_temp(fr, argument_value_ref, 8, 8);  // TODO: size of pointer

// 					instruction in = {0};
// 					in.type = INS_LEA;
// 					in.as.op.dst = t2;
// 					in.as.op.src1 = t1;
// 					array_append(&instructions, in);

// 					in = (instruction){0};
// 					in.type = INS_COPY;
// 					in.as.op.dst = t2;
// 					in.as.op.src1 = t0;
// 					in.as.op.src2 = CLITERAL(field.size);
// 					array_append(&instructions, in);
// 				}

// 				return t1;
// 			} else if (tree.op == operator_index) {
// 				if (tree.operands.ptr[0].type != expression_type_identifier) {
// 					report_compiler_error(loc, sv("unexpected array access"));
// 					return argument_error;
// 				}

// 				symbol* symbl = symbol_lookup(tree.operands.ptr[0].as.identifier);
// 				if (symbl == NULL) {
// 					report_compiler_error(loc, tconcat(sv("unknown symbol "), tree.operands.ptr[0].as.identifier));
// 					return argument_error;
// 				}

// 				if (symbl->type.type == type_array) {
// 					argument base = symbol_to_argument(loc, symbl);
// 					if (base.type == argument_none) {
// 						return argument_error;
// 					}

// 					argument index_arg = compile_expression(loc, tree.operands.ptr[1], fr);
// 					if (index_arg.type == argument_none) {
// 						return argument_error;
// 					}

// 					size_t elm_size = size_of_type(*symbl->type.as.array.inner);
// 					size_t elm_alignment = size_of_type(*symbl->type.as.array.inner);
// 					argument t0 = frame_allocate_temp(fr, argument_value_ref, 8, 8);  // TODO: size of pointer
// 					argument_value_type value_type = (elm_size<=8)?argument_value_int:argument_value_aggregate;
// 					argument t1 = frame_allocate_temp(fr, value_type, elm_size, elm_alignment);

// 					instruction in = {0};
// 					in.type = INS_LEA;
// 					in.as.op.dst = t0;
// 					in.as.op.src1 = base;
// 					array_append(&instructions, in);

// 					in = (instruction){0};
// 					in.type = INS_MADD;
// 					in.as.op.dst = t0;
// 					in.as.op.src1 = index_arg;
// 					in.as.op.src2 =	CLITERAL(elm_size);
// 					in.as.op.src3 = t0;
// 					array_append(&instructions, in);

// 					// TODO: support bigger sizes
// 					if (elm_size <= 8) {
// 						in = (instruction){0};
// 						in.type = INS_LOAD;
// 						in.as.op.dst = t1;
// 						in.as.op.src1 = t0;
// 						array_append(&instructions, in);
// 					} else {
// 						argument t2 = frame_allocate_temp(fr, argument_value_ref, 8, 8);  // TODO: size of pointer

// 						instruction in = {0};
// 						in.type = INS_LEA;
// 						in.as.op.dst = t2;
// 						in.as.op.src1 = t1;
// 						array_append(&instructions, in);

// 						in = (instruction){0};
// 						in.type = INS_COPY;
// 						in.as.op.dst = t2;
// 						in.as.op.src1 = t0;
// 						in.as.op.src2 = CLITERAL(elm_size);
// 						array_append(&instructions, in);
// 					}

// 					return t1;
// 				} else if (symbl->type.type == type_primitive && symbl->type.as.primitive == primitive_string) {
// 					// string = struct { size_t len, size_t ptr}
// 					argument base = symbol_to_argument(loc, symbl);
// 					if (base.type == argument_none) {
// 						return argument_error;
// 					}
// 					// TODO: casting???
// 					argument ptr_arg = argument_field(base, 8, 8);
// 					ptr_arg.value_type = argument_value_ref;

// 					argument index_arg = compile_expression(loc, tree.operands.ptr[1], fr);
// 					if (index_arg.type == argument_none) {
// 						return argument_error;
// 					}

// 					argument t0 = frame_allocate_temp(fr, argument_value_ref, 8, 8);  // TODO: size of pointer
// 					argument t1 = frame_allocate_temp(fr, argument_value_int, 1, 1); // TODO: size of char

// 					instruction in = {0};
// 					in.type = INS_LOAD;
// 					in.as.op.dst = t0;
// 					in.as.op.src1 = ptr_arg;
// 					array_append(&instructions, in);

// 					in = (instruction){0};
// 					in.type = INS_ADD;
// 					in.as.op.dst = t0;
// 					in.as.op.src1 = t0;
// 					in.as.op.src2 = index_arg;
// 					array_append(&instructions, in);

// 					in = (instruction){0};
// 					in.type = INS_LOAD;
// 					in.as.op.dst = t1;
// 					in.as.op.src1 = t0;
// 					array_append(&instructions, in);

// 					return t1;
// 				} else {
// 					report_compiler_error(loc, tconcat(sv("invalid index op "), symbl->identifier));
// 					return argument_error;
// 				}
// 			} else {
// 				// TODO: typecheck
// 				instruction in = {0};

// 				in.as.op.src1 = compile_expression(loc, tree.operands.ptr[0], fr);
// 				if (in.as.op.src1.type == argument_none) return argument_error;
				
// 				argument_value_type value_type = (in.as.op.src1.size<=8)?argument_value_int:argument_value_aggregate;
// 				in.as.op.dst = frame_allocate_temp(fr, value_type, in.as.op.src1.size, in.as.op.src1.alignment);

// 				switch(expr.as.tree.op) {
// 				case operator_plus:
// 					in.type = INS_ADD;
// 					break;
// 				case operator_minus:
// 					in.type = INS_SUB;
// 					break;
// 				case operator_star:
// 					in.type = INS_MUL;
// 					break;
// 				case operator_slash:
// 					in.type = INS_DIV;
// 					break;
// 				default: unreachable;
// 				}

// 				in.as.op.src2 = compile_expression(loc, tree.operands.ptr[1], fr);
// 				if (in.as.op.src2.type == argument_none) return argument_error;

// 				array_append(&instructions, in);

// 				return in.as.op.dst;
// 			}
// 		} else unreachable;
// 	} break;
// 	default: todo("compile_expression");;
// 	}
// }

// void compile_statement(statement stm, frame* fr) {
// 	switch(stm.type) {
// 	case statement_type_scope: {
// 		statement_scope sc = stm.as.scope;
// 		for (int i=0; i<sc.statements.len; i++) {
// 			compile_statement(sc.statements.ptr[i], fr);
// 		}
// 	} break;
// 	case statement_type_decl: {
// 		declaration decl = stm.as.declaration;
// 		if (symbol_lookup(decl.identifier) != NULL) {
// 			report_compiler_error(stm.loc, tconcat(sv("redefintion of symbol "), decl.identifier));
// 			return;
// 		}

// 		struct type decl_type;
// 		size_t size;
// 		size_t alignment;

// 		if (decl.type.type == type_struct) {
// 			symbol* struct_symbol = symbol_lookup(decl.type.as.structure.identifier);
// 			if (struct_symbol == NULL) {
// 				report_compiler_error(stm.loc, tconcat(sv("unknown identifier "), decl.type.as.structure.identifier));
// 				return;
// 			}
// 			decl_type = struct_symbol->type;
// 			size = struct_symbol->size;
// 			alignment = struct_symbol->alignment;
// 		} else {
// 			decl_type = decl.type;
// 			size = size_of_type(decl_type);
// 			alignment = alignment_of_type(decl_type);
// 		}

// 		argument_value_type value_type = (size<=8)?argument_value_int:argument_value_aggregate;
// 		argument dst = frame_allocate(fr, argument_local, value_type, decl.identifier, size, alignment);
// 		symbol_add(symbol_type_local, decl.identifier, decl_type, dst.offset, size, alignment);

// 		if (decl.value.type != expression_type_none) {
// 			argument src = compile_expression(stm.loc, decl.value, fr);
// 			if (src.type == argument_none) {
// 				return;
// 			}

// 			// TODO: typecheck
// 			if (src.size != dst.size) {
// 				report_compiler_error(stm.loc, sv("size mismatch in assignment"));
// 				return;
// 			}

// 			if (src.size <= 8) {
// 				instruction in = (instruction){0};
// 				in.type = INS_STORE;
// 				in.as.op.dst = dst;
// 				in.as.op.src1 = src;
// 				array_append(&instructions, in);
// 			} else {
// 				argument t0 = frame_allocate_temp(fr, argument_value_ref, 8, 8);
// 				argument t1 = frame_allocate_temp(fr, argument_value_ref, 8, 8);

// 				instruction in = (instruction){0};
// 				in.type = INS_LEA;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = dst;
// 				array_append(&instructions, in);

// 				in = (instruction){0};
// 				in.type = INS_LEA;
// 				in.as.op.dst = t1;
// 				in.as.op.src1 = src;
// 				array_append(&instructions, in);

// 				in = (instruction){0};
// 				in.type = INS_COPY;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = t1;
// 				in.as.op.src2 = CLITERAL(src.size);
// 				array_append(&instructions, in);
// 			}
// 		}
// 	} break;
// 	case statement_type_assign: {
// 		statement_assign assign = stm.as.assignment;

// 		argument src = compile_expression(stm.loc, assign.value, fr);
// 		if (src.type == argument_none) {
// 			return;
// 		}

// 		symbol* symbl = symbol_lookup(assign.destination.identifier);
// 		if (symbl == NULL) {
// 			report_compiler_error(stm.loc, tconcat(sv("unknown identifier "), assign.destination.identifier));
// 			return;
// 		}

// 		if (assign.destination.type == destination_type_variable) {
// 			argument dst = symbol_to_argument(stm.loc, symbl);
// 			if (dst.type == argument_none) {
// 				return;
// 			}

// 			// TODO: typecheck
// 			if (src.size != dst.size) {
// 				report_compiler_error(stm.loc, sv("size mismatch in assignment"));
// 				return;
// 			}

// 			if (src.size <= 8) {
// 				instruction in = (instruction){0};
// 				in.type = INS_STORE;
// 				in.as.op.dst = dst;
// 				in.as.op.src1 = src;
// 			} else {
// 				argument t0 = frame_allocate_temp(fr, argument_value_ref, 8, 8);
// 				argument t1 = frame_allocate_temp(fr, argument_value_ref, 8, 8);

// 				instruction in = (instruction){0};
// 				in.type = INS_LEA;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = dst;
// 				array_append(&instructions, in);

// 				in = (instruction){0};
// 				in.type = INS_LEA;
// 				in.as.op.dst = t1;
// 				in.as.op.src1 = src;
// 				array_append(&instructions, in);

// 				in = (instruction){0};
// 				in.type = INS_COPY;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = t1;
// 				in.as.op.src2 = CLITERAL(src.size);
// 				array_append(&instructions, in);
// 			}
// 		} else if (assign.destination.type == destination_type_indexed) {
// 			if (symbl->type.type != type_array) {
// 				report_compiler_error(stm.loc, sv("object must be an array to be indexed"));
// 				return;
// 			}

// 			size_t elm_size;
// 			if (symbl->type.as.array.inner->type == type_struct) {
// 				symbol* struct_symbol = symbol_lookup(symbl->type.as.array.inner->as.structure.identifier);
// 				if (struct_symbol == NULL) {
// 					report_compiler_error(stm.loc, tconcat(sv("unknown identifier "), symbl->type.as.array.inner->as.structure.identifier));
// 					return;
// 				}
// 				elm_size = struct_symbol->size;
// 			} else {
// 				elm_size = size_of_type(*symbl->type.as.array.inner);
// 			}

// 			// TODO: typecheck
// 			if (src.size != elm_size) {
// 				report_compiler_error(stm.loc, sv("size mismatch in assignment"));
// 				return;
// 			}

// 			argument base = symbol_to_argument(stm.loc, symbl);
// 			if (base.type == argument_none) {
// 				return;
// 			}

// 			argument t0 = frame_allocate_temp(fr, argument_value_ref, 8, 8);

// 			instruction in = (instruction){0};
// 			in.type = INS_LEA;
// 			in.as.op.dst = t0;
// 			in.as.op.src1 = base;
// 			array_append(&instructions, in);

// 			argument index_arg = compile_expression(stm.loc, assign.destination.as.indexed.value, fr);
// 			if (index_arg.type == argument_none) {
// 				return;
// 			}

// 			in = (instruction){0};
// 			in.type = INS_MADD;
// 			in.as.op.dst = t0;
// 			in.as.op.src1 = index_arg;
// 			in.as.op.src2 =	CLITERAL(elm_size);
// 			in.as.op.src3 = t0;
// 			array_append(&instructions, in);
		
// 			if (src.size <= 8) {
// 				in = (instruction){0};
// 				in.type = INS_STORE;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = src;
// 				array_append(&instructions, in);
// 			} else {
// 				ASSERT(src.type != argument_literal);
// 				argument t1 = frame_allocate_temp(fr, argument_value_ref, 8, 8);
				
// 				instruction in = (instruction){0};
// 				in.type = INS_LEA;
// 				in.as.op.dst = t1;
// 				in.as.op.src1 = src;
// 				array_append(&instructions, in);

// 				in = (instruction){0};
// 				in.type = INS_COPY;
// 				in.as.op.dst = t0;
// 				in.as.op.src1 = t1;
// 				in.as.op.src2 = CLITERAL(src.size);
// 				array_append(&instructions, in);
// 			}
// 		} else if (assign.destination.type == destination_type_field) {
// 			if (symbl->type.type != type_struct) {
// 				report_compiler_error(stm.loc, sv("object must be an struct to be access field"));
// 				return;
// 			}

// 			string field_name = assign.destination.as.field.name;
// 			struct_field field = field_of_struct(symbl->type.as.structure, field_name);
// 			if (field.type.type == type_none) {
// 				report_compiler_error(stm.loc, tconcat(sv("unknown field "), field_name));
// 				return;
// 			}

// 			// TODO: typecheck
// 			if (src.size != field.size) {
// 				report_compiler_error(stm.loc, sv("size mismatch in assignment"));
// 				return;
// 			}

// 			argument base = symbol_to_argument(stm.loc, symbl);
// 			if (base.type == argument_none) {
// 				return;
// 			}

// 			argument t0 = frame_allocate_temp(fr, argument_value_ref, 8, 8);

// 			instruction in = (instruction){0};
// 			in.type = INS_LEA;
// 			in.as.op.dst = t0;
// 			in.as.op.src1 = base;
// 			array_append(&instructions, in);

// 			in = (instruction){0};
// 			in.type = INS_ADD;
// 			in.as.op.dst = t0;
// 			in.as.op.src1 = t0;
// 			in.as.op.src2 =	CLITERAL(field.offset);
// 			array_append(&instructions, in);
// 		} else unreachable;

// 	} break;
// 	case statement_type_return: {
// 		argument arg = compile_expression(stm.loc, stm.as.ret.value, fr);
// 		if (arg.type == argument_none) {
// 			return;
// 		}
// 		if (arg.size > 4) {
// 			report_compiler_error(stm.loc, sv("expression must be integer"));
// 			return;
// 		}
// 		instruction in = (instruction){.type = INS_RET, .as = {.op = {.dst=arg}}};
// 		array_append(&instructions, in);
// 	} break;
// 	default: todo("implement compile_statement");
// 	}
// }

// void load_function_params(function fn, frame* fr) {
// 	int offset = 0;
// 	for (int i=0; i<fn.arguments.len; i++) {
// 		binding bind = fn.arguments.ptr[i];

// 		size_t size;
// 		size_t alignment;
// 		struct type decl_type;
// 		if (bind.type.type == type_struct) {
// 			symbol* struct_symbol = symbol_lookup(bind.type.as.structure.identifier);
// 			if (struct_symbol == NULL) {
// 				report_compiler_error(fn.loc, tconcat(sv("unknown identifier "), bind.type.as.structure.identifier));
// 				return;
// 			}
// 			decl_type = struct_symbol->type;
// 			size = struct_symbol->size;
// 			alignment = struct_symbol->alignment;
// 		} else {
// 			decl_type = bind.type;
// 			size = size_of_type(decl_type);
// 			alignment = alignment_of_type(decl_type);
// 		}

// 		argument arg = (argument){.type=argument_param, .name=bind.identifier, .offset=offset, .size=size, .alignment=alignment};
// 		if (arg.size <= 8) offset++; else offset+=2;
		
// 		instruction in	= {0};
// 		in.type = INS_LPARAM;
// 		argument_value_type value_type = (arg.size<=8)?argument_value_int:argument_value_aggregate;
// 		in.as.op.dst = frame_allocate(fr, argument_local, value_type, bind.identifier, arg.size, arg.alignment);
// 		in.as.op.src1 = arg;

// 		array_append(&instructions, in);

// 		symbol_add(symbol_type_local, bind.identifier, decl_type, in.as.op.dst.offset, size, alignment);
// 	}
// }

// void compile_function(function fn) {
// 	symbol* symbl = symbol_lookup(fn.identifier);
// 	if (symbl != NULL) {
// 		report_compiler_error(fn.loc, tconcat(sv("redefinition of symbl "), fn.identifier));
// 		return;
// 	}
// 	symbol_add(symbol_type_function, fn.identifier, type_of_function(fn), 0, 8, 1);

// 	frame* fr = calloc(1, sizeof(frame));
// 	symbol_table saved = symbols;

// 	instruction in = (instruction){.type = INS_FUNC_START, .label=fn.identifier, .as = {.func = {.frame=fr}}};
// 	array_append(&instructions, in);
	
// 	load_function_params(fn, fr);

// 	for (int i=0; i<fn.body.len; i++) {
// 		compile_statement(fn.body.ptr[i], fr);
// 	}
// 	symbols.first = saved.first;
// 	symbols.last = saved.last;

// 	in = (instruction){.type = INS_FUNC_END, .label=fn.identifier};
// 	array_append(&instructions, in);
// }

// void compile_structure(structure strukt) {
// 	symbol* symbl = symbol_lookup(strukt.identifier);
// 	if (symbl != NULL) {
// 		report_compiler_error(strukt.loc, tconcat(sv("redefinition of symbl "), strukt.identifier));
// 		return;
// 	}

// 	type strukt_type = type_of_struct(strukt);	
// 	size_t size = size_of_type(strukt_type);
// 	size_t alignment = alignment_of_type(strukt_type);

// 	symbol_add(symbol_type_struct, strukt.identifier, strukt_type, 0, size, alignment);
// }
