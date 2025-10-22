typedef enum {
	argument_type_literal,
	argument_type_temp,
	argument_type_local_var,
	argument_type_global_var,
} argument_type;

typedef struct {
	argument_type type;
	union {
		struct {
			size_t size;
			size_t offset;
		} var;
		struct {
			size_t size;
			size_t value;
		} literal;
	} as;
} argument;

typedef enum {
	INS_ASSIGN,
	INS_INTRINSIC,
	INS_FUNC_START,
	INS_FUNC_END,
	INS_RETURN,
	INS_PUSH_SCOPE,
	INS_POP_SCOPE,
	INS_CALL,
	INS_SYSCALL
} instruction_type;

// Store the value in 
typedef struct {
	argument destination;
	argument value;
} instruction_assign;

typedef struct {
	string identifier;
} instruction_func;

typedef struct {
	size_t size;
} instruction_scope;

typedef struct {
	argument value;
} instruction_return;

typedef enum {
	intrinsic_add,
	intrinsic_sub,
	intrinsic_mul,
	intrinsic_div
} intrinsic_type;

typedef struct {
	intrinsic_type type;
	int argc;
	argument* args;
} instruction_intrinsic;

typedef struct {
	instruction_type type;
	union {
		instruction_assign assign;
		instruction_func func;
		instruction_scope scope;
		instruction_return ret;
		instruction_intrinsic intrinsic;
	} as;
} instruction;

typedef struct {
	int len;
	int cap;
	instruction* ptr;
} instruction_list;

// Symbols are stored in a strmap

typedef enum {
	symbol_type_local,
	symbol_type_global,
} symbol_type;

typedef struct symbol symbol;

typedef struct symbol {
	symbol_type type;
	string identifier;
	size_t offset;
	size_t size;

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

void symbol_add(symbol_list* symbols, symbol_type type, string identifier, size_t offset, size_t size) {
	symbol* symbl = malloc(sizeof(symbol));
	symbl->type = type;
	symbl->identifier = identifier;
	symbl->offset = offset;
	symbl->size = size;
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

static instruction_list instructions = {0};

void report_compiler_error(string message) {
	print(sv("error: compilation failed: "));
	println(message);
	exit(1);
}

argument argument_temp() {
	argument arg = {0};
	arg.type = argument_type_temp;
	return arg;
}

argument argument_local_var(size_t offset, size_t size) {
	argument arg = {0};
	arg.type = argument_type_local_var;
	arg.as.var.offset = offset;
	arg.as.var.size = size;
	return arg;
}

argument argument_global_var(size_t offset, size_t size) {
	argument arg = {0};
	arg.type = argument_type_global_var;
	arg.as.var.offset = offset;
	arg.as.var.size = size;
	return arg;
}

argument argument_literal(size_t size, size_t value) {
	argument arg = {0};
	arg.type = argument_type_literal;
	arg.as.literal.size = size;
	arg.as.literal.value = value;
	return arg;
}

int size_of_type(type typ) {
	switch(typ.type) {
	case type_primitive: {
		switch(typ.as.primitive) {
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
	case type_array:
	case type_wrapped:
	case type_struct:
	case type_none: unreachable
	}
}

intrinsic_type intrinsic_from_expression(expression_tree tree) {
	if (tree.operand_count == 2) {
		switch (tree.op) {
		case operator_plus: return intrinsic_add;
		case operator_minus: return intrinsic_sub;
		case operator_star: return intrinsic_mul;
		case operator_slash: return intrinsic_div;
		default: unreachable;	
		}
	} else if (tree.operand_count == 1) {
		unreachable;
	}
	unreachable;
}

argument compile_expression(expression expr, symbol_list* symbols) {
	switch(expr.type) {
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_integer: {
			return argument_literal(4, string_to_number(expr.as.literal.value));
		}
		default: unreachable;
		}
	} break;
	case expression_type_identifier: {
		symbol* symbl = symbol_lookup(symbols, expr.as.identifier.identifier);
		if (symbl == NULL) {
			report_compiler_error(tconcat(sv("unknown symbol "), expr.as.identifier.identifier));
		}
		if (symbl->type == symbol_type_global) {
			return argument_global_var(symbl->offset, symbl->size);
		} else {
			return argument_local_var(symbl->offset, symbl->size);
		}
	} break;
	case expression_type_tree: {
		instruction ins = {0};
		ins.type = INS_INTRINSIC;

		ins.as.intrinsic.type = intrinsic_from_expression(expr.as.tree);
		ins.as.intrinsic.argc = expr.as.tree.operand_count;
		ins.as.intrinsic.args = malloc(ins.as.intrinsic.argc * sizeof(argument)); 
		for (int i=0; i<expr.as.tree.operand_count; i++) {
			ins.as.intrinsic.args[i] = compile_expression(expr.as.tree.operands[i], symbols);
		}
		array_append(&instructions, ins);

		return argument_temp();
	} break;
	case expression_type_none: unreachable;
	}
}

void push_scope(size_t size) {
	instruction ins = {0};
	ins.type = INS_PUSH_SCOPE;
	ins.as.scope.size = size;
	array_append(&instructions, ins);
}

void pop_scope(size_t size) {
	instruction ins = {0};
	ins.type = INS_POP_SCOPE;
	ins.as.scope.size = size;
	array_append(&instructions, ins);
}

void compile_scope(scope sc, symbol_list* symbols) {
	size_t stack_size = 0;
	for (int i=0; i<sc.statements.len; i++) {
		statement stm = sc.statements.ptr[i];
		if (stm.type == statement_type_decl) 
			stack_size += size_of_type(stm.as.declaration.type);
	}
	push_scope(stack_size);
	symbol_list saved = *symbols;

	size_t offset = 0;
	for (int i=0; i<sc.statements.len; i++) {
		statement stm = sc.statements.ptr[i];
		switch(stm.type) {
		case statement_type_scope: {
			compile_scope(stm.as.scope, symbols);
		} break;
		case statement_type_decl: {
			size_t size = size_of_type(stm.as.declaration.type);

			if (stm.as.declaration.value.type != expression_type_none) {
				instruction ins = {0};
				ins.type = INS_ASSIGN;
				ins.as.assign.destination = argument_local_var(offset, size);
				ins.as.assign.value = compile_expression(stm.as.declaration.value, symbols);
				array_append(&instructions, ins);
			}

			symbol_add(symbols, symbol_type_local, stm.as.declaration.identifier, offset, size);
			offset += size;
		} break;
		case statement_type_assign: {
			symbol* symbl = symbol_lookup(symbols, stm.as.assignment.identifier);
			if (symbl == NULL) {
				report_compiler_error(tconcat(sv("unknown symbol "), stm.as.assignment.identifier));
			}

			instruction ins = {0};
			ins.type = INS_ASSIGN;
			if (symbl->type == symbol_type_global) {
				ins.as.assign.destination = argument_global_var(symbl->offset, symbl->size);
			} else {
				ins.as.assign.destination = argument_local_var(symbl->offset, symbl->size);
			}
			ins.as.assign.value = compile_expression(stm.as.assignment.value, symbols);

			array_append(&instructions, ins);
		} break;
		case statement_type_return: {
			argument result = compile_expression(stm.as.ret.value, symbols);

			symbols->first = saved.first;
			symbols->last = saved.last;
			pop_scope(stack_size);

			instruction ins = {0};
			ins.type = INS_RETURN;
			ins.as.ret.value = result;
			array_append(&instructions, ins);
			return;
		} break;
		default: unreachable;
		}
	}

	symbols->first = saved.first;
	symbols->last = saved.last;
	pop_scope(stack_size);
}

void compile_function(function fn, symbol_list* symbols) {
	instruction ins = {0};
	ins.type = INS_FUNC_START;
	ins.as.func.identifier = fn.identifier;
	array_append(&instructions, ins);

	compile_scope(fn.scope, symbols);

	instruction ins_end = {0};
	ins_end.type = INS_FUNC_END;
	ins_end.as.func.identifier = fn.identifier;
	array_append(&instructions, ins_end);
}

instruction_list compile(program prg) {
	symbol_list symbols = {0};

	for (int i=0; i<prg.functions.len; i++) {
		function fn = prg.functions.ptr[i];
		compile_function(fn, &symbols);
	}
	return instructions;
}