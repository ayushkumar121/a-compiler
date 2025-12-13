typedef enum {
	builtin_print,
	builtin_exit,
	builtin_count
} builtin;

typedef enum {
	ins_label,
	ins_binop,
	ins_func_start,
	ins_func_end,
	ins_func_call,
	ins_ret,
	ins_jmp,
	ins_jmp_ifnot,
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
	// stores the value of src1 to pointer in dst, stores src2 bytes
	op_store_indirect,
	// copies src2 bytes from src1 to dst
	op_copy,
} op_type;

typedef enum {
	VR0,
	VR1,
	VR2,
	VR3,
	vreg_count
} virtual_register;

typedef enum {
	argument_type_none,
	argument_type_vreg,
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
		virtual_register vreg;
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
		frame* frame;
		argument ret;
		string label;
		struct {op_type type; argument dst, src1, src2;} op;
		struct {argument dst; string identifier; int argc; argument* args;} fcall;
		struct {string label;} jmp;
		struct {argument cond; string label;} jmpifnot;
	} as;
} instruction;

typedef struct {
	int len;
	int cap;
	instruction* ptr;
} instruction_list;

typedef struct {
	strings string_literals;
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
#define WORD_SIZE 4

symbol* symbol_top = NULL;
symbol* symbol_saved = NULL;
strings string_literals = {0};
instruction_list instructions = {0};
lexer_file_loc loc;
string current_func_name;
string current_func_name_end;

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
		char* symbol_type = (iter->symbol_type == symbol_type_local)? "local":"global";
		printf(sfmt" : "sfmt" @ %s %d\n", sarg(iter->identifier), sarg(type_to_string(*iter->type)), symbol_type, iter->offset);
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

// top = [1] -> [2] -> null
// saved = [2] -> null
void symbol_restore(symbol* saved) {
	ASSERT(symbol_top != NULL);
	symbol* s = symbol_top;
	while(s != saved && s != NULL) {
		symbol* next = s->next;
		type_free(s->type);
		free(s);
		s = next;
	}
	symbol_top = saved;
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
	case type_none: unreachable();
	}
	unreachable();
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
		case primitive_none: unreachable();
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

inline static void add_instruction_jmp(string label) {
	array_append(&instructions, ((instruction){.type=ins_jmp, .as = {.jmp={label=label}}}));
}

inline static void add_instruction_jmpifnot(argument cond, string label) {
	array_append(&instructions, ((instruction){.type=ins_jmp_ifnot, .as = {.jmpifnot={.cond=cond, .label=label}}}));
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

inline static argument argument_register(virtual_register vreg) {
	return (argument){.type=argument_type_vreg, .size=PTR_SIZE, .as={.vreg=vreg}};
}

inline static argument argument_string_literal(string str) {
	argument arg = (argument){.type=argument_type_string, .size=PTR_SIZE, .as={.index=string_literals.len}};
	array_append(&string_literals, str);
	return arg;
}

inline static argument argument_literal(int value) {
	return (argument){.type=argument_type_literal, .size=WORD_SIZE, .as={.value=value}};
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
    int offset = frame->size + alignment;
    frame->size = offset;
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
	else unreachable();
}

argument argument_field(argument base, int offset, int size) {
	if (base.type == argument_type_local) base.as.offset -= offset;
	else if (base.type == argument_type_global) base.as.offset += offset;
	else unreachable();
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
	case type_none: unreachable();
	}
	unreachable();
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
	case expression_type_func_call: {
		symbol* symbl = symbol_lookup(expr.as.func_call.identifier, symbol_type_function);
		ASSERT(symbl && symbl->type);
		return *symbl->type->as.function.return_type;
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
			default: unreachable();
			}
		} else if (expr.as.tree.operands.len == 2) {
			switch(expr.as.tree.op) {
			case operator_plus:
			case operator_minus:
			case operator_star:
			case operator_slash:
				return type_of_expression(expr.as.tree.operands.ptr[0]);
			case operator_index: {
				type base_type = type_of_expression(expr.as.tree.operands.ptr[0]);
				if (base_type.type == type_array) {
					return *base_type.as.array.inner;
				} else if (base_type.type == type_primitive && base_type.as.primitive == primitive_string) {
					return type_of_primitive(primitive_byte);
				} else unreachable();
			}
			default: unreachable();
			}
		} else unreachable();
	}
	default: unreachable();
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
			argument t0 = argument_register(VR0);
			add_instruction_op(op_addrof, t0, base);
			add_instruction_op2(op_madd, t0, index, argument_literal(elemsize));
			add_instruction_op2(op_load_indirect, dst, t0, argument_literal(elemsize));
			return dst;
		} else if (decl_type->type == type_primitive && decl_type->as.primitive == primitive_string) {
			// string = {size_t, char*}
			argument data = argument_field(argument_from_symbol(symbl), PTR_SIZE, PTR_SIZE);
			argument index = compile_expression(frame, expr_tree.operands.ptr[1]);
			if (!index.type) return argument_none();

			argument t0 = argument_register(VR0);
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

		return argument_field(argument_from_symbol(symbl), f.offset, f.size);
	} else {
		op_type op;
		switch(expr_tree.op) {
		case operator_plus: op = op_add; break;
		case operator_minus: op = op_sub; break;
		case operator_star: op = op_mul; break;
		case operator_slash: op = op_div; break;
		default: unreachable();
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
	} else unreachable();
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
		} else unreachable();
	}
	case expression_type_none: return argument_none();
	}
	unreachable();
}

void compile_statement(frame* frame, statement stm);

void compile_statement_list(frame* frame, statement_list stms) {
	for (int i=0; i<stms.len; i++) {
		compile_statement(frame, stms.ptr[i]);
	}
}

void compile_assignment(frame* frame, statement_assign assignment) {
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

		type* elemtype = decl_type->as.array.inner;
		type value_type = type_of_expression(assignment.value);
		if (!type_eq(elemtype, &value_type)) {
			report_compiler_error(loc, type_mismatch_error(*elemtype, value_type));
			return;
		}

		int elemsize = size_of_type(decl_type->as.array.inner);
		argument base = argument_from_symbol(symbl);
		argument index = compile_expression(frame, assignment.lvalue.as.index);
		if (index.type == argument_type_none) return;
		argument value = compile_expression(frame, assignment.value);
		if (value.type == argument_type_none) return;

		argument t0 = argument_register(VR0);
		add_instruction_op(op_addrof, t0, base);
		add_instruction_op2(op_madd, t0, index, argument_literal(elemsize));

		if (elemsize <= PTR_SIZE)
			add_instruction_op2(op_store_indirect, t0, value, argument_literal(elemsize));
		else {
			argument t1 = argument_register(VR1);
			add_instruction_op(op_addrof, t1, value);
			add_instruction_op2(op_copy, t0, t1, argument_literal(elemsize));
		}
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

		type value_type = type_of_expression(assignment.value);
		if (!type_eq(&f.type, &value_type)) {
			report_compiler_error(loc, type_mismatch_error(f.type, value_type));
			return;
		}

		argument dst = argument_field(argument_from_symbol(symbl), f.offset, f.size);
		argument src = compile_expression(frame, assignment.value);
		if (src.type == argument_type_none) return;

		if (dst.size <= PTR_SIZE)
			add_instruction_op(op_store, dst, src);
		else 
			add_instruction_op2(op_copy, dst, src, argument_literal(dst.size));
	} break;

	default: unreachable();
	}
}

void compile_statement(frame* frame, statement stm) {
	loc = stm.loc;
	switch(stm.type) {
	case statement_type_list: {
		symbol* saved = symbol_top;
		compile_statement_list(frame, stm.as.list);
		symbol_restore(saved);
	} break;

	case statement_type_decl: {
		declaration decl = stm.as.declaration;
		type decl_type = resolve_complete_type(decl.type);
		if (decl_type.type == type_none) {
			report_compiler_error(loc, tconcat(sv("unknown type "), type_to_string(decl_type)));
			return;
		}

		argument dst;
		if (decl.value.type != expression_type_none) {
			type value_type = type_of_expression(decl.value);
			if (!type_eq(&decl_type, &value_type)) {
				report_compiler_error(loc, type_mismatch_error(decl_type, value_type));
				return;
			}

			argument src = compile_expression(frame, decl.value);
			if (src.type == argument_type_none) return;
			if (src.type == argument_type_local) {
				// Incase src is already allocated on local we steal its location
				dst = src;
			} else {
				dst = argument_allocate(frame, size_of_type(&decl_type));
				if (dst.size <= PTR_SIZE)
					add_instruction_op(op_store, dst, src);
				else 
					add_instruction_op2(op_copy, dst, src, argument_literal(dst.size));
			}
		} else {
			dst = argument_allocate(frame, size_of_type(&decl_type));
		}
		symbol_add(symbol_type_local, decl.identifier, decl_type, dst.as.offset);
	} break;

	case statement_type_assign: {
		compile_assignment(frame, stm.as.assignment);
	} break;

	case statement_type_return: {
		argument value;

		if (stm.as.ret.value.type != expression_type_none) {
			symbol* symbl = symbol_lookup(current_func_name, symbol_type_function);
			ASSERT(symbl != NULL);

			type* return_type = symbl->type->as.function.return_type;
			type value_type = type_of_expression(stm.as.ret.value);
			if (!type_eq(return_type, &value_type)) {
				report_compiler_error(loc, type_mismatch_error(*return_type, value_type));
				return;
			}

			value = compile_expression(frame, stm.as.ret.value);
			if (value.type == argument_type_none) return;
		} else {
			value = argument_none();
		}
		add_instruction_ret(value);
		add_instruction_jmp(current_func_name_end);
	} break;

	case statement_type_if: {
		argument cond = compile_expression(frame, stm.as.iff.condition);
		if (cond.type == argument_type_none) return;
		if (cond.size > PTR_SIZE) {
			report_compiler_error(loc, sv("condition must be integeral"));
			return;
		}

		static int if_counter = 0;
		string else_label = tsprintf("else%d", if_counter);
		string end_label = tsprintf("end%d", if_counter);
		if_counter++;

		// if only case
		if (stm.as.iff.else_body.len == 0) {
			add_instruction_jmpifnot(cond, end_label);
			compile_statement_list(frame, stm.as.iff.iff_body);
		} else { // if else case
			add_instruction_jmpifnot(cond, else_label);
			compile_statement_list(frame, stm.as.iff.iff_body);
			add_instruction_jmp(end_label);
			add_instruction_label(else_label);
			compile_statement_list(frame, stm.as.iff.else_body);
		}
		add_instruction_label(end_label);
	} break;

	case statement_type_func_call: {
		(void)compile_fcall(frame, stm.as.func_call);
	} break;

	default: unreachable();
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
	current_func_name = func.identifier;
	current_func_name_end = strconcat(func.identifier, sv("_end"));

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
	symbol_restore(saved);
	add_instruction_label(current_func_name_end);
	add_instruction_func_end(frame);
}

void compile_builtin(builtin b) {
	switch(b) {
	case builtin_print: {
		type typ = {0};
		typ.type = type_function;
		typ.as.function.return_type = malloc(sizeof(type));
		typ.as.function.identifier = sv("print");
		*(typ.as.function.return_type) = type_of_primitive(primitive_void);
		array_append(&typ.as.function.arguments, type_of_primitive(primitive_string));
		symbol_add(symbol_type_function, typ.as.function.identifier, typ, 0);
	} break;

	case builtin_exit: {
		type typ = {0};
		typ.type = type_function;
		typ.as.function.return_type = malloc(sizeof(type));
		typ.as.function.identifier = sv("exit");
		*(typ.as.function.return_type) = type_of_primitive(primitive_void);
		array_append(&typ.as.function.arguments, type_of_primitive(primitive_int));
		symbol_add(symbol_type_function, typ.as.function.identifier, typ, 0);
	} break;

	case builtin_count: break;
	}
}

intermediate_representation compile(program prg) {
	fprintf(stderr, "info: compiling program\n");

	for (int i = 0; i <builtin_count; ++i){
		compile_builtin(i);
	}

	for (int i=0; i<prg.structs.len; i++) {
		compile_structure(prg.structs.ptr[i]);
	}
	structs_free(prg.structs);

	for (int i=0; i<prg.functions.len; i++) {
		compile_function(prg.functions.ptr[i]);
	}
	functions_free(prg.functions);

	//symbol_restore(NULL);
	return (intermediate_representation){string_literals, instructions};
}
