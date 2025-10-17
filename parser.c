// begin types

typedef struct type type;
typedef struct paramter paramter;
typedef struct paramter_list paramter_list;
typedef struct expression expression;
typedef struct expression_list expression_list;
typedef struct statement statement;
typedef struct block block;

typedef struct {
	int len;
	int cap;
	type* ptr;
} type_list;

typedef enum {
	type_primitive,
	type_wrapped,
	type_array,
	type_struct,
	type_function,
} type_type;

typedef enum {
	primitive_void,
	primitive_byte,
	primitive_ubyte,
	primitive_int,
	primitive_uint,
	primitive_short,
	primitive_ushort,
	primitive_long,
	primitive_ulong,
	primitive_float,
	primitive_double,
} primitive_type;

typedef enum {
	wrapped_type_pointer,
	wrapped_type_optional,
	wrapped_type_result,
} wrapped_type_type;

typedef struct {
	wrapped_type_type type;
	type* inner;
} wrapped_type;

typedef struct {
	int size; // 0 for dynamic arrays
	type* inner;
} array_type;

typedef struct {
	string identifier;
} struct_type;

typedef struct {
	type* return_type;
	type_list arguments;
} function_type;

typedef struct type {
	type_type type;
	union {
		primitive_type primitive;
		wrapped_type wrapped;
		array_type array;
		struct_type strukt;
		function_type function;
	} as;
} type;

// end types

typedef struct parameter {
	type type;
	string identifier;
} parameter;

typedef struct paramter_list {
	int len;
	int cap;
	parameter* ptr;
} parameter_list;

typedef enum {
	expression_type_literal,
	expression_type_variable,
} expression_type;

typedef enum {
	expression_literal_integer,
	expression_literal_string,
} expression_literal_type;

typedef struct {
	expression_literal_type type;
	string value;
} expression_literal;

typedef struct {
	string identifier;
} expression_variable;

typedef struct expression {
	expression_type type;
	union {
		expression_literal literal;
		expression_variable variable;
	} as;
} expression;

typedef struct expression_list {
	int len;
	int cap;
	expression *ptr;
} expression_list;

typedef enum {
	statement_type_decl,
	statement_type_assign,
	statement_type_func_call,
	statement_type_block,
} statement_type;

typedef struct {
	bool constant;
	type type;
	string identifier;
	expression value;
} statement_decl;

typedef struct {
	string identifier;
	expression value;
} statement_assign;

typedef struct {
	expression_list expressions;
} statement_func_call;

typedef struct block {
	struct {
		int len;
		int cap;
		statement* ptr;
	} statements;
} statement_block;

typedef struct statement {
	statement_type type;
	union {
		statement_decl declaration;
		statement_assign assignment;
		statement_func_call func_call;
		statement_block block;
	} as;
} statement;

// typedef struct statement_list {
// 	int len;
// 	int cap;
// 	statement* ptr;
// } statement_list;

typedef struct {
	string identifier;
	type return_type;
	parameter_list parameters;
	statement_block block;
} function;

typedef struct {
	int len;
	int cap;
	function* ptr;
} function_list;

typedef struct {
	string identifier;
	parameter_list parameters;
} strukt;

typedef struct {
	int len;
	int cap;
	strukt* ptr;
} struct_list;

typedef struct {
	struct_list structs;
	function_list funcs;
} program;


// Error reporting
void report_error(lexer* lex, string message, const char* file, int line) {
	lexer_file_loc loc = lexer_current_loc(lex); 
	fprintf(stderr, "%.*s:%d:%d error: %.*s compiler:%s:%d\n", 
		string_arg(loc.file), loc.line, loc.col, string_arg(message), file, line);
}

#define report_error_and_exit(lex, message) report_error_and_exit_(lex, message, __FILE__, __LINE__)

#define report_todo_and_exit(lex, feature) report_error_and_exit(lex, tprintf("todo %s", feature));

void report_error_and_exit_(lexer* lex, string message, const char* file, int line) {
	report_error(lex, message, file, line);
	__builtin_trap();  
	exit(1);
}

string keyword_to_string(keyword_type keyword) {
    switch (keyword) {
    case keyword_void: return sv("void");
    case keyword_int: return sv("int");
    case keyword_uint: return sv("uint");
    case keyword_byte: return sv("byte");
    case keyword_ubyte: return sv("ubyte");
    case keyword_short: return sv("short");
    case keyword_ushort: return sv("ushort");
    case keyword_long: return sv("long");
    case keyword_ulong: return sv("ulong");
    case keyword_float: return sv("float");
    case keyword_double: return sv("double");
    case keyword_error: return sv("error");
    case keyword_typ: return sv("typ");
    case keyword_static: return sv("static");
    case keyword_var: return sv("var");
    case keyword_const: return sv("const");
    case keyword_enum: return sv("enum");
    case keyword_if: return sv("if");
    case keyword_else: return sv("else");
    case keyword_for: return sv("for");
    case keyword_foreach: return sv("foreach");
    case keyword_while: return sv("while");
    case keyword_switch: return sv("switch");
    case keyword_case: return sv("case");
    case keyword_default: return sv("default");
    case keyword_break: return sv("break");
    case keyword_continue: return sv("continue");
    case keyword_return: return sv("return");
    case keyword_extern: return sv("extern");
    case keyword_struct: return sv("struct");
    case keyword_func: return sv("func");
    case keyword_union: return sv("union");
    case keyword_import: return sv("import");
    case keyword_nil: return sv("nil");
    }
}

string primitive_to_string(primitive_type primitive) {
	switch(primitive) {
	case primitive_void: return sv("void");
	case primitive_int: return sv("int");
	case primitive_uint: return sv("uint");
	case primitive_byte: return sv("byte");
	case primitive_ubyte: return sv("ubyte");
	case primitive_short: return sv("short");
	case primitive_ushort: return sv("ushort");
	case primitive_long: return sv("long");
	case primitive_ulong: return sv("ulong");
	case primitive_float: return sv("float");
	case primitive_double: return sv("double");
	}
}

string wrapped_to_string(wrapped_type_type typ) {
	switch(typ) {
	case wrapped_type_pointer: return sv("*");
	case wrapped_type_optional: return sv("?");
	case wrapped_type_result: return sv("!");
	}
}

string type_to_string(type typ) {
	switch(typ.type) {
	case type_primitive: return primitive_to_string(typ.as.primitive);
	case type_wrapped: return tconcat(wrapped_to_string(typ.as.wrapped.type), type_to_string(*typ.as.wrapped.inner));
	case type_array: return tprintf("[%d]%.*s",typ.as.array.size, string_arg(type_to_string(*typ.as.array.inner)));
	case type_struct: return typ.as.strukt.identifier;
	case type_function: {
		string s = sv("func(");
		for (int i=0; i<typ.as.function.arguments.len; i++) {
			s = tconcat(s, type_to_string(typ.as.function.arguments.ptr[i]));
			if (i < typ.as.function.arguments.len-1) {
				s = tconcat(s, sv(","));
			}
		}
		s = tconcat(s, sv(")"));
		assert(typ.as.function.return_type != NULL);
		s = tconcat(s, type_to_string(*typ.as.function.return_type));
		return s;
	}
	}
}

// Parsers

// primitive = int,float,...
primitive_type parse_primitive(lexer* lex, keyword_type keyword) {
	switch(keyword) {
	case keyword_void: return primitive_void;
	case keyword_int: return primitive_int;
	case keyword_uint: return primitive_uint;
	case keyword_byte: return primitive_byte;
	case keyword_ubyte: return primitive_ubyte;
	case keyword_short: return primitive_short;
	case keyword_ushort: return primitive_ushort;
	case keyword_long: return primitive_long;
	case keyword_ulong: return primitive_ulong;
	case keyword_float: return primitive_float;
	case keyword_double: return primitive_double;
	default: report_error_and_exit(lex, tconcat(sv("expected a type but got "), lex->current_token.value));
	}
	assert(0);
}

// type = (*)(|?|!|[n]|[])(int|indef) | func(type...)type
type parse_type(lexer* lex) {
	type typ = {0};

	token t = lexer_next_token(lex);
	if (t.type == token_star) {
		typ.type = type_wrapped;
		typ.as.wrapped.type = wrapped_type_pointer;
		typ.as.wrapped.inner = malloc(sizeof(type));
		*(typ.as.wrapped.inner) = parse_type(lex);
	} else if (t.type == token_question) {
		typ.type = type_wrapped;
		typ.as.wrapped.type = wrapped_type_optional;
		typ.as.wrapped.inner = malloc(sizeof(type));
		*(typ.as.wrapped.inner) = parse_type(lex);
	} else if (t.type == token_exclaimation) {
		typ.type = type_wrapped;
		typ.as.wrapped.type = wrapped_type_result;
		typ.as.wrapped.inner = malloc(sizeof(type));
		*(typ.as.wrapped.inner) = parse_type(lex);
	} else if (t.type == token_left_bracket) {
		typ.type = type_array;

		t = lexer_next_token(lex);
		if (t.type == token_integer) {
			char* num = talloc(t.value.len);
			memcpy(num, t.value.ptr, t.value.len);
			typ.as.array.size = atoi(num);

			t = lexer_next_token(lex);
			assert(t.type == token_right_bracket);
		} else {
			assert(t.type == token_right_bracket);
		}

		typ.as.wrapped.inner = malloc(sizeof(type));
		*(typ.as.array.inner) = parse_type(lex);
	} else if (t.type == token_keyword) {
		typ.type = type_primitive;
		typ.as.primitive = parse_primitive(lex, t.keyword);
	} else if (t.type == token_identifier) {
		typ.type = type_struct;
		typ.as.strukt.identifier = t.value;
	} else if (t.type == token_keyword && t.keyword == keyword_func) {
		typ.type = type_function;
		assert(0 && "parsing of function type is not implemented");
	} else {
		 report_error_and_exit(lex, tconcat(sv("unexpected token "), t.value));
	}

	return typ;
}

// param = type ident
parameter parse_parameter(lexer* lex) {
	parameter param = {0};
	token t = lexer_next_token(lex);
	if (t.type != token_identifier) {
		report_error_and_exit(lex, tconcat(sv("expected identifier but got "), t.value));
	}
	param.identifier = t.value;
	param.type = parse_type(lex);
	return param;
}

expression parse_expression(lexer* lex) {
	expression ex = {0};
	token t = lexer_next_token(lex);
	if (t.type == token_integer) {
		ex.type = expression_type_literal;
		ex.as.literal.type = expression_literal_integer;
		ex.as.literal.value = t.value;
	} else if (t.type == token_string) {
		ex.type = expression_type_literal;
		ex.as.literal.type = expression_literal_string;
		ex.as.literal.value = t.value;
	} else if (t.type == token_identifier) {
		ex.type = expression_type_variable;
		ex.as.variable.identifier = t.value;
	} else {
		report_todo_and_exit(lex, "expression parsing");
	}

	return ex;
}

statement parse_statement(lexer* lex) {
	statement s = {0};

	token t = lexer_next_token(lex);
	if (t.type == token_keyword && (t.keyword == keyword_var || t.keyword == keyword_const)) {
		s.type = statement_type_decl;
		s.as.declaration.constant = t.keyword == keyword_const;

		parameter param = parse_parameter(lex);
		s.as.declaration.type = param.type;
		s.as.declaration.identifier = param.identifier;

		t = lexer_next_token(lex);
		if (t.type == token_equal) {
			s.as.declaration.value = parse_expression(lex);
		} else {
			lexer_rewind(lex);
		}

		t = lexer_next_token(lex);
		if (t.type != token_semicolon) {
			report_error_and_exit(lex, tconcat(sv("expected ; but got "), t.value));
		}
	} else if (t.type == token_identifier) {
		string identifier = t.value;
		t = lexer_next_token(lex);
		if (t.type == token_equal) {
			s.type = statement_type_assign;
			s.as.assignment.identifier = identifier;
			s.as.assignment.value = parse_expression(lex);
		} else if (t.type == token_left_paren) {
			s.type = statement_type_func_call;
			while(true) {
				t = lexer_next_token(lex);
				if (t.type == token_right_paren) break;
				lexer_rewind(lex);

				expression ex = parse_expression(lex);
				array_append(&(s.as.func_call.expressions), ex);

				t = lexer_next_token(lex);
				if (t.type == token_comma) continue;
				else if (t.type == token_right_paren) break;
				else {
					report_error_and_exit(lex, tprintf("expected ) got token `%.*s`", string_arg(t.value)));
				}
			}
		} else {
			report_todo_and_exit(lex, "parse complex assignments");
		}

		t = lexer_next_token(lex);
		if (t.type != token_semicolon) {
			report_error_and_exit(lex, tconcat(sv("expected ; but got "), t.value));
		}
	} else if (t.type == token_left_curly) {
		s.type = statement_type_block;
		while(true) {
			t = lexer_next_token(lex);
			if (t.type == token_right_curly) break;
			lexer_rewind(lex);

			statement st = parse_statement(lex);
			array_append(&(s.as.block.statements), st);
		}
	} else {
		report_error_and_exit(lex, tconcat(sv("unexpected token "), t.value));
	}

	return s;
}

function parse_func(lexer* lex) {
	function func = {0};

	// Parsing identifier
	token t = lexer_next_token(lex);
	if (t.type != token_identifier) {
		report_error_and_exit(lex, tconcat(sv("expected identifier but got "), t.value));
	}
	func.identifier = t.value;

	t = lexer_next_token(lex);
	if (t.type == token_less_than) {
		report_todo_and_exit(lex, "generic parsing in functions");
	}

	if (t.type == token_left_paren) {
		// Parsing parameter list
		while(true) {
			t = lexer_next_token(lex);
			if (t.type == token_right_paren) break;
			lexer_rewind(lex);

			parameter param = parse_parameter(lex);
 			array_append(&func.parameters, param);

 			t = lexer_next_token(lex);
 			if (t.type == token_comma) continue;
 			else if (t.type == token_right_paren) break;
 			else {
 				report_error_and_exit(lex, tconcat(sv("expected ) but got "), t.value));
 			}
		}
	} else {
		 report_error_and_exit(lex, tconcat(sv("unexpected token "), t.value));
	}

	t = lexer_next_token(lex);
	if (t.type != token_left_curly) {
		lexer_rewind(lex);
		func.return_type = parse_type(lex);
	} else {
		func.return_type.type = type_primitive;
		func.return_type.as.primitive = primitive_void;

		while(true) {
			t = lexer_next_token(lex);
			if (t.type == token_right_curly) break;
			lexer_rewind(lex);

			statement st = parse_statement(lex);
			array_append(&(func.block.statements), st);
		}
	}

	return func;
}

strukt parse_struct(lexer* lex) {
	strukt s = {0};

	// Parsing identifier
	token t = lexer_next_token(lex);
	if (t.type != token_identifier) {
		report_error_and_exit(lex, tconcat(sv("expected identifier but got "), t.value));
	}
	s.identifier = t.value;

	t = lexer_next_token(lex);
	if (t.type == token_less_than) {
		report_todo_and_exit(lex, "generic parsing in structs");
	}

	if (t.type == token_left_curly) {
		while(true) {
			t = lexer_next_token(lex);
			if (t.type == token_right_curly) break;
			lexer_rewind(lex);

			parameter param = parse_parameter(lex);
 			array_append(&s.parameters, param);

 			t = lexer_next_token(lex);
 			if (t.type == token_semicolon) continue;
 			else if (t.type == token_right_curly) break;
 			else {
		 		report_error_and_exit(lex, tconcat(sv("expected } but got "), t.value));
 			}
		}
	} else {
		report_error_and_exit(lex, tconcat(sv("unexpected token "), t.value));
	}

	return s;
}

program parse_program(lexer* lex) {
	program prg = {0};
	token t = lexer_next_token(lex);
	while (t.type != token_error) {
		if (t.type == token_keyword && t.keyword == keyword_func) {
			function f = parse_func(lex);
			array_append(&prg.funcs, f);
		} else if (t.type == token_keyword && t.keyword == keyword_struct) {
			strukt s = parse_struct(lex);
			array_append(&prg.structs, s);
		} else {
			report_error_and_exit(lex, tprintf("encountered unknown token `%.*s` at top level", string_arg(t.value)));
		}
		t = lexer_next_token(lex);
	}
	__builtin_trap();
	return prg;
}