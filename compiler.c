typedef enum {
	argument_type_none,
	argument_type_literal,
	argument_type_local_var,
	argument_type_global_var,
} argument_type;

typedef enum {
	argument_literal_type_integer,
	argument_literal_type_string,
} argument_literal_type;

typedef struct {
	argument_type type;
	size_t size;
	size_t stride;
	union {
		struct {
			size_t offset;
		} var;
		struct {
			argument_literal_type type;
			union {
				struct {int64_t value;} integer;
				struct {size_t offset; size_t len;} string;
			} as;
		} literal;
	} as;
} argument;

#define argument_error (argument){0}

typedef enum {
	INS_ASSIGN,
	INS_INTRINSIC,
	INS_FUNC_START,
	INS_FUNC_END,
	INS_RETURN,
	INS_LABEL,
	INS_IF,
	INS_WHILE,
	INS_END_WHILE,
	INS_CALL,
	INS_SYSCALL
} instruction_type;

typedef struct {
	argument destination;
	argument value;
	argument index;
} instruction_assign;

typedef struct {
	string identifier;
	size_t* stack_size;
} instruction_func;

typedef struct {
	argument value;
} instruction_return;

typedef enum {
	intrinsic_add,
	intrinsic_sub,
	intrinsic_mul,
	intrinsic_div,
	intrinsic_ref,
	intrinsic_index,
} intrinsic_type;

typedef struct {
	int index;
} intrinsic_label;

typedef struct {
	argument destination;
	intrinsic_type type;
	int argc;
	argument* args;
} instruction_intrinsic;

typedef struct {
	argument destination;
	string identifier;
	int argc;
	argument* args;
} instruction_func_call;

typedef struct {
	int label;
	argument condition;
} instruction_jmp;

typedef struct {
	instruction_type type;
	union {
		instruction_assign assign;
		instruction_func func;
		instruction_return ret;
		instruction_intrinsic intrinsic;
		instruction_func_call func_call;
		instruction_jmp jmp;
		intrinsic_label label;
	} as;
} instruction;

typedef struct {
	int len;
	int cap;
	instruction* ptr;
} instruction_list;

typedef enum {
	global_value_type_none,
	global_value_type_number,
	global_value_type_string,
	global_value_type_array,
} global_value_type;

typedef struct {
	global_value_type type;
	union {
		size_t number;
		struct {size_t offset; size_t size;} string; // offset into string arrays
		struct {int len; char* data;} array;
	} as;
} global_value;

typedef struct {
	size_t size;
	string identifier;
	global_value value;
} global;

typedef struct {
	int len;
	int cap;
	global* ptr;
} global_list;

typedef struct {
	strings strings;
	global_list globals;
	instruction_list instructions;
} intermediate_representation;

typedef enum {
	symbol_type_local_var,
	symbol_type_global_var,
	symbol_type_func,
	symbol_type_struct,
} symbol_type;

typedef struct symbol symbol;

typedef struct symbol {
	string identifier;
	type type;
	symbol_type symbol_type;
	size_t offset;

	symbol* next;
	symbol* prev;
} symbol;

typedef struct {
	symbol* first;
	symbol* last;
} symbol_list;

void symbol_table_print(symbol_list* symbols) {
	symbol* symbl = symbols->first;
	while(symbl != NULL) {
		print(symbl->identifier);
		print(sv("->"));
		symbl = symbl->next;
	}
	println(sv(""));
}

void symbol_add(symbol_list* symbols, symbol_type symbol_type, string identifier, type type, size_t offset) {
	symbol* symbl = malloc(sizeof(symbol));
	symbl->type = type;
	symbl->identifier = identifier;
	symbl->offset = offset;
	symbl->symbol_type = symbol_type;
	symbl->prev = NULL;
	symbl->next = NULL;

	if (symbols->last == NULL) {
		symbols->first = symbl;
		symbols->last = symbl;
	} else {
		symbols->last->next = symbl;
		symbl->prev = symbols->last;
		symbols->last = symbl;
	}
}

symbol* symbol_lookup(symbol_list* symbols, string identifier) {
	if(symbols->last != NULL) {
		symbol* symbl = symbols->last;
		while(symbl != NULL) {
			if (string_eq(symbl->identifier, identifier)) {
				return symbl;
			} 
			symbl = symbl->prev;
		}
	}
	return NULL;
}

strings string_literals = {0};
instruction_list instructions = {0};
global_list globals = {0};

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
		unreachable;
	}
	case type_function: return 8; // function_ptr;
	case type_array: return type.as.array.size * size_of_type(*type.as.array.inner);
	case type_slice:
	case type_wrapped:
	case type_struct:
		todo("implement size_of_type")
	case type_none: unreachable
	}
}

size_t stride_of_type(type type) {
	if (type.type == type_array) {
		return size_of_type(*type.as.array.inner);
	}
	return size_of_type(type);
}

argument argument_local_var(size_t offset, size_t size, size_t stride) {
	argument arg = {0};
	arg.type = argument_type_local_var;
	arg.size = size;
	arg.stride = stride;
	arg.as.var.offset = offset;
	return arg;
}

argument argument_global_var(size_t offset, size_t size, size_t stride) {
	argument arg = {0};
	arg.type = argument_type_global_var;
	arg.size = size;
	arg.stride = stride;
	arg.as.var.offset = offset;
	return arg;
}

argument argument_literal_integer(size_t size, size_t value) {
	argument arg = {0};
	arg.type = argument_type_literal;
	arg.size = size;
	arg.as.literal.type = argument_literal_type_integer;
	arg.as.literal.as.integer.value = value;
	return arg;
}

argument argument_literal_string(size_t offset, size_t len) {
	argument arg = {0};
	arg.type = argument_type_literal;
	arg.size = 16;
	arg.as.literal.type = argument_literal_type_string;
	arg.as.literal.as.string.offset = offset;
	arg.as.literal.as.string.len = len;
	return arg;
}

bool is_constant_expression(expression expr) {
	return expr.type == expression_type_literal;
}

bool is_numeric_expression(expression expr, symbol_list* symbols) {
	switch(expr.type) {
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_integer:
		case expression_literal_char: return true;
		case expression_literal_string: return false;
		}
	}
	case expression_type_identifier: {
		symbol* symbl = symbol_lookup(symbols, expr.as.identifier.identifier);
		if (symbl == NULL) return false;
		return size_of_type(symbl->type) <= 8; // convertible to word
	}
	case expression_type_tree: {
		for (int i=0; i<expr.as.tree.operands.len; i++) {
			if (!is_numeric_expression(expr.as.tree.operands.ptr[i], symbols)) 
				return false;
		}
		return true;
	}
	case expression_type_func_call: todo("this needs updating");
	case expression_type_none: unreachable;
	}
}

intrinsic_type intrinsic_from_expression(expression_tree tree) {
	if (tree.operands.len == 2) {
		switch (tree.op) {
		case operator_plus: return intrinsic_add;
		case operator_minus: return intrinsic_sub;
		case operator_star: return intrinsic_mul;
		case operator_slash: return intrinsic_div;
		case operator_index: return intrinsic_index;
		default: unreachable;	
		}
	} else if (tree.operands.len == 1) {
		switch (tree.op) {
		case operator_ampersand: return intrinsic_ref;
		default: unreachable;	
		}
	}
	unreachable;
}

type type_of_struct(structure s) {
	type typ = {0};
	typ.type = type_struct;
	typ.as.structure.identifier = s.identifier;
	for (int i=0; i<s.parameters.len; i++) {
		array_append(&typ.as.structure.parameters, s.parameters.ptr[i].type);
	}
	return typ;
}

type type_of_function(function fn) {
	type typ = {0};
	typ.type = type_function;
	typ.as.function.return_type = malloc(sizeof(type));
	*(typ.as.function.return_type) = fn.return_type;
	for (int i=0; i<fn.arguments.len; i++) {
		array_append(&typ.as.function.arguments, fn.arguments.ptr[i].type);
	}
	return typ;
}

type type_of_primitive(primitive_type primitive) {
	type typ = {0};
	typ.type = type_primitive;
	typ.as.primitive = primitive;
	return typ;
}

type type_of_expression(expression expr, symbol_list* symbols) {
	switch(expr.type) {
	case expression_type_literal: {
		switch(expr.as.literal.type) {
			case expression_literal_integer: return type_of_primitive(primitive_int);
			case expression_literal_char: return type_of_primitive(primitive_byte);
			case expression_literal_string: return type_of_primitive(primitive_string);
		}
	}
	case expression_type_identifier: {
		symbol* symbl = symbol_lookup(symbols, expr.as.identifier.identifier);
		if (symbl == NULL) return type_error;
		return symbl->type;
	}
	case expression_type_func_call: {
		symbol* symbl = symbol_lookup(symbols, expr.as.func_call.identifier);
		if (symbl == NULL) return type_error;
		return symbl->type;
	}
	case expression_type_tree: 
	case expression_type_none: unreachable;
	}
}

bool type_equal(type a, type b) {
	if (a.type == b.type) {
		switch(a.type) {
		case type_primitive: return a.as.primitive == b.as.primitive;
		case type_wrapped: 
			return a.as.wrapped.type == b.as.wrapped.type && 
			type_equal(*a.as.wrapped.inner, *b.as.wrapped.inner);
		case type_slice: 
			return type_equal(*a.as.slice.inner, *b.as.slice.inner);
		case type_array:
			return a.as.array.size == b.as.array.size &&
			type_equal(*a.as.array.inner, *b.as.array.inner);
		case type_struct:
		case type_function: 
		case type_none: unreachable;
		}
	}
	return false;
}

argument compile_expression(lexer_file_loc loc, expression expr,  size_t destination_size, size_t* stack_size, symbol_list* symbols) {
	switch(expr.type) {
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_integer: {
			return argument_literal_integer(4, expr.as.literal.as.integer);
		}
		case expression_literal_char: {
			return argument_literal_integer(1, expr.as.literal.as.character);
		}
		case expression_literal_string: {
			argument arg = argument_literal_string(string_literals.len, expr.as.literal.as.string.len);
			array_append(&string_literals, expr.as.literal.as.string);
			return arg;
		}
		}
	} break;
	case expression_type_identifier: {
		symbol* symbl = symbol_lookup(symbols, expr.as.identifier.identifier);
		if (symbl == NULL) {
			report_compiler_error(loc, tconcat(sv("unknown symbol "), expr.as.identifier.identifier));
			return argument_error;
		}

		size_t size = size_of_type(symbl->type);
		size_t stride = stride_of_type(symbl->type);
		size_t offset = symbl->offset;

		if (symbl->symbol_type == symbol_type_global_var) {
			return argument_global_var(offset, size, stride);
		} else {
			return argument_local_var(offset, size, stride);
		}
	} break;
	case expression_type_func_call: {
		size_t offset = *stack_size;
		*stack_size  = *stack_size + destination_size; 


		argument destination = argument_local_var(offset, destination_size, destination_size);
		instruction ins = {0};
		ins.type = INS_CALL;

		ins.as.func_call.destination = destination;
		ins.as.func_call.identifier = expr.as.func_call.identifier;
		ins.as.func_call.argc = expr.as.func_call.expressions.len;
		ins.as.func_call.args = malloc(ins.as.intrinsic.argc * sizeof(argument)); 
		for (int i=0; i<ins.as.func_call.argc; i++) {
			ins.as.func_call.args[i] = compile_expression(loc, expr.as.func_call.expressions.ptr[i], destination_size, stack_size, symbols);
		}
		array_append(&instructions, ins);

		return destination;
	} break;
	case expression_type_tree: {
		size_t offset = *stack_size;
		*stack_size  = *stack_size + destination_size;

		argument destination = argument_local_var(offset, destination_size, destination_size);
		instruction ins = {0};
		ins.type = INS_INTRINSIC;

		ins.as.intrinsic.destination = destination;
		ins.as.intrinsic.type = intrinsic_from_expression(expr.as.tree);
		ins.as.intrinsic.argc = expr.as.tree.operands.len;
		ins.as.intrinsic.args = malloc(ins.as.intrinsic.argc * sizeof(argument)); 
		for (int i=0; i<ins.as.intrinsic.argc; i++) {
			ins.as.intrinsic.args[i] = compile_expression(loc, expr.as.tree.operands.ptr[i], destination_size, stack_size, symbols);
		}
		array_append(&instructions, ins);

		return destination;
	} break;
	case expression_type_none: unreachable;
	}
}

void add_label(int label_index) {
	instruction ins = {0};
	ins.type = INS_LABEL;
	ins.as.label.index = label_index;
	array_append(&instructions, ins);
}

void compile_scope(scope sc, size_t* stack_size, symbol_list* symbols) {
	symbol_list saved = *symbols;

	for (int i=0; i<sc.statements.len; i++) {
		statement stm = sc.statements.ptr[i];

		switch(stm.type) {
		case statement_type_scope: {
			compile_scope(stm.as.scope, stack_size, symbols);
		} break;
		case statement_type_decl: {
			size_t offset = *stack_size;
			size_t size = size_of_type(stm.as.declaration.type);
			size_t stride = stride_of_type(stm.as.declaration.type);
			*stack_size = *stack_size + size;

			symbol_add(symbols, symbol_type_local_var, stm.as.declaration.identifier, stm.as.declaration.type, offset);

			if (stm.as.declaration.value.type != expression_type_none) {
				if (!type_equal(stm.as.declaration.type, type_of_expression(stm.as.declaration.value, symbols))) {
					report_compiler_error(stm.loc, tconcat(sv("expected value to be of type "), type_to_string(stm.as.declaration.type)));
					continue;
				}

				instruction ins = {0};
				ins.type = INS_ASSIGN;
				ins.as.assign.destination = argument_local_var(offset, size, stride);
				ins.as.assign.value = compile_expression(stm.loc, stm.as.declaration.value, size, stack_size, symbols);
				array_append(&instructions, ins);
			}
		} break;
		case statement_type_assign: {
			destination dest = stm.as.assignment.destination;
			string identifier = dest.identifier;
			symbol* symbl = symbol_lookup(symbols, identifier);
			if (symbl == NULL) {
				report_compiler_error(stm.loc, tconcat(sv("unknown symbol "), identifier));
				continue;
			}

			type symbl_type = symbl->type;
			instruction ins = {0};
			ins.type = INS_ASSIGN;

			if (dest.type == destination_type_indexed) {
				if (symbl_type.type != type_array) {
					report_compiler_error(stm.loc, tconcat(sv("cannot index into type "), type_to_string(symbl_type)));
					continue;
				}
				ins.as.assign.index = compile_expression(stm.loc, dest.as.indexed.value, 4, stack_size, symbols);

				type inner_type = *symbl_type.as.array.inner;
				if (!type_equal(inner_type, type_of_expression(stm.as.assignment.value, symbols))) {
					report_compiler_error(stm.loc, tconcat(sv("expected value to be of type "), type_to_string(inner_type)));
					continue;
				}
			} else {
				ins.as.assign.index.type = argument_type_none;
				if (!type_equal(symbl_type, type_of_expression(stm.as.assignment.value, symbols))) {
					report_compiler_error(stm.loc, tconcat(sv("expected value to be of type "), type_to_string(symbl_type)));
					continue;
				}
			}

			size_t size = size_of_type(symbl->type);
			size_t stride = stride_of_type(symbl->type);
			size_t offset = symbl->offset;

			if (symbl->symbol_type == symbol_type_global_var) {
				ins.as.assign.destination = argument_global_var(offset, size, stride);
			} else {
				ins.as.assign.destination = argument_local_var(offset, size, stride);
			}
			ins.as.assign.value = compile_expression(stm.loc, stm.as.assignment.value, size, stack_size, symbols);
			array_append(&instructions, ins);
		} break;
		case statement_type_return: { // Return also pops scope
			argument result = compile_expression(stm.loc, stm.as.ret.value, 4, stack_size, symbols);

			symbols->first = saved.first;
			symbols->last = saved.last;

			instruction ins = {0};
			ins.type = INS_RETURN;
			ins.as.ret.value = result;
			array_append(&instructions, ins);
			return;
		} break;
		case statement_type_func_call:  {
			instruction ins = {0};
			ins.type = INS_CALL;
			ins.as.func_call.identifier = stm.as.func_call.identifier;
			ins.as.func_call.argc = stm.as.func_call.expressions.len;
			ins.as.func_call.args = malloc(ins.as.func_call.argc * sizeof(argument)); 
			for (int i=0; i<ins.as.func_call.argc; i++) {
				ins.as.func_call.args[i] = compile_expression(stm.loc, stm.as.func_call.expressions.ptr[i], 4, stack_size, symbols);
			}
			array_append(&instructions, ins);
		} break;
		case statement_type_if: {
			instruction ins = {0};
			ins.type = INS_IF;
			if (!is_numeric_expression(stm.as.iff.condition, symbols)) {
				report_compiler_error(stm.loc, sv("if condition needs to be numeric"));
				continue;
			}
			ins.as.jmp.condition = compile_expression(stm.loc, stm.as.iff.condition, 4, stack_size, symbols);
			ins.as.jmp.label = 1;
			array_append(&instructions, ins);

			add_label(0);
			compile_scope(stm.as.iff.body, stack_size, symbols);
			add_label(1);
		} break;
		case statement_type_while: {
			add_label(0);

			instruction ins = {0};
			ins.type = INS_WHILE;
			if (!is_numeric_expression(stm.as.whil.condition, symbols)) {
				report_compiler_error(stm.loc, sv("while condition needs to be numeric"));
				continue;
			}
			ins.as.jmp.condition = compile_expression(stm.loc, stm.as.whil.condition, 4, stack_size, symbols);
			ins.as.jmp.label = 1;
			array_append(&instructions, ins);

			compile_scope(stm.as.whil.body, stack_size, symbols);

			ins.type = INS_END_WHILE;
			ins.as.jmp.label = 0;
			array_append(&instructions, ins);

			add_label(1);
		} break;
		case statement_type_none: unreachable;
		}
	}

	symbols->first = saved.first;
	symbols->last = saved.last;
}

void compile_function(function fn, symbol_list* symbols) {
	size_t* stack_size = calloc(1, sizeof(size_t));
	instruction ins = {0};
	ins.type = INS_FUNC_START;
	ins.as.func.identifier = fn.identifier;
	ins.as.func.stack_size = stack_size;
	array_append(&instructions, ins);

	compile_scope(fn.body, stack_size, symbols);

	instruction ins_end = {0};
	ins_end.type = INS_FUNC_END;
	ins_end.as.func.identifier = fn.identifier;
	ins_end.as.func.stack_size = stack_size;
	array_append(&instructions, ins_end);
}

void compile_global_decl(declaration decl, size_t size) {
	global glb = {.size=size, .identifier=decl.identifier};
	if (decl.value.type != expression_type_none) {
		if (is_constant_expression(decl.value)) {
			switch (decl.value.as.literal.type) {
			case expression_literal_integer: {
				glb.value.type = global_value_type_number;
				glb.value.as.number = decl.value.as.literal.as.integer;
			} break;
			case expression_literal_char: {
				glb.value.type = global_value_type_number;
				glb.value.as.number = decl.value.as.literal.as.character;
			} break;
			case expression_literal_string: {
				glb.value.type = global_value_type_string;
				glb.value.as.string.offset = string_literals.len;
				glb.value.as.string.size = decl.value.as.literal.as.string.len;
				array_append(&string_literals, decl.value.as.literal.as.string);
			} break;
			}
		} else {
			report_error_old(tconcat(
				decl.identifier, sv(" is static varibale and can only be initialised with a constant expressions")));
			exit(1);
		}
	} else {
		glb.value.type = global_value_type_none;
	}
	array_append(&globals, glb);
}

intermediate_representation compile(program prg) {
	intermediate_representation ir = {0};
	symbol_list symbols = {0};

	size_t offset = 0;
	for (int i=0; i<prg.globals.len; i++) {
		declaration decl = prg.globals.ptr[i];
		symbol_add(&symbols, symbol_type_global_var, decl.identifier, decl.type, offset);
		
		size_t size = size_of_type(decl.type);
		offset += size;

		compile_global_decl(decl, size);
	}

	for (int i=0; i<prg.functions.len; i++) {
		function fn = prg.functions.ptr[i];
		compile_function(fn, &symbols);

		type func_type = type_of_function(fn);
		symbol_add(&symbols, symbol_type_func, fn.identifier, func_type, i);
	}

	for (int i=0; i<prg.structs.len; i++) {
		structure strukt = prg.structs.ptr[i];
		type strukt_type = type_of_struct(strukt);
		symbol_add(&symbols, symbol_type_func, strukt.identifier, strukt_type, i);
	}

	ir.strings = string_literals;
	ir.instructions = instructions;
	ir.globals = globals;
	return ir;
}
