// begin types

typedef struct type type;
typedef struct expression expression;
typedef struct statement statement;
typedef struct scope scope;
typedef struct binding binding;
typedef struct expression expression;

typedef struct {
	int len;
	int cap;
	type* ptr;
} type_list;

typedef enum {
	type_none,
	type_primitive,
	type_wrapped,
	type_slice,
	type_array,
	type_struct,
	type_function,
} type_type;

typedef enum {
	primitive_none,
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
	primitive_string,
} primitive_type;

typedef enum {
	wrapped_type_constant,
	wrapped_type_pointer,
	wrapped_type_optional,
	wrapped_type_result,
} wrapped_type_type;

typedef struct {
	wrapped_type_type type;
	type* inner;
} wrapped_type;

typedef struct {
	int size;
	type* inner;
} array_type;

typedef struct {
	type* inner;
} slice_type;

typedef struct {
	string identifier;
	type_list parameters;
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
		slice_type slice;
		struct_type structure;
		function_type function;
	} as;
} type;

#define type_error (type){.type=type_none}

// end types

typedef struct binding {
	type type;
	string identifier;
} binding;

typedef struct {
	int len;
	int cap;
	binding* ptr;
} binding_list;

typedef enum {
	operator_plus,
	operator_minus,
	operator_star,
	operator_slash,
	operator_dot,
	operator_ampersand,
	operator_question,
	operator_exclaimation,
	operator_index,
	operator_none,
} operator;

typedef enum {
	expression_type_none,
	expression_type_literal,
	expression_type_func_call,
	expression_type_identifier,
	expression_type_tree,
} expression_type;

typedef enum {
	expression_literal_integer,
	expression_literal_string,
	expression_literal_char,
} expression_literal_type;

typedef struct {
	expression_literal_type type;
	union {
		string string;
		char character;
		int64_t integer;
	} as;
} expression_literal;

typedef struct {
	string identifier;
} expression_variable;

typedef struct expression_list {
	int len;
	int cap;
	expression *ptr;
} expression_list;

typedef struct {
	string identifier;
	expression_list expressions;
} expression_func_call;

typedef struct {
	operator op;
	expression_list operands;
} expression_tree;

typedef struct expression {
	expression_type type;
	union {
		expression_literal literal;
		expression_variable identifier;
		expression_tree tree;
		expression_func_call func_call;
	} as;
} expression;

#define expression_error (expression){.type=expression_type_none};

typedef struct {
	bool error;
	type type;
	string identifier;
	expression value;
	lexer_file_loc loc;
} declaration;

#define declaration_error (declaration){.error=true};

typedef struct {
	int len;
	int cap;
	declaration* ptr;
} declaration_list;

typedef enum {
	statement_type_decl,
	statement_type_assign,
	statement_type_func_call,
	statement_type_scope,
	statement_type_return,
	statement_type_if,
	statement_type_while,
	statement_type_none,
} statement_type;

typedef enum {
	destination_type_variable,
	destination_type_indexed,
} destination_type;

typedef struct {
	destination_type type;
	string identifier;
	union {
		struct {expression value;} indexed;
		struct {} var;
	} as;
} destination;

typedef struct {
	destination destination;
	expression value;
} statement_assign;

typedef struct {
	string identifier;
	expression_list expressions;
} statement_func_call;

typedef struct scope {
	struct {
		int len;
		int cap;
		statement* ptr;
	} statements;
} statement_scope;

typedef struct {
	expression value;
} statement_return;

typedef struct {
	expression condition;
	statement_scope body;
} statement_if;

typedef struct {
	expression condition;
	statement_scope body;
} statement_while;

typedef struct statement {
	statement_type type;
	union {
		declaration declaration;
		statement_assign assignment;
		statement_func_call func_call;
		statement_scope scope;
		statement_return ret;
		statement_if iff;
		statement_while whil;
	} as;
	lexer_file_loc loc;
} statement;

#define statement_error (statement){.type=statement_type_none};

typedef struct {
	bool error;
	string identifier;
	type return_type;
	binding_list arguments;
	statement_scope body;
	lexer_file_loc loc;
} function;

#define function_error (function){.error=true};

typedef struct {
	int len;
	int cap;
	function* ptr;
} function_list;

typedef struct {
	bool error;
	string identifier;
	binding_list parameters;
	lexer_file_loc loc;
} structure;

typedef struct {
	int len;
	int cap;
	structure* ptr;
} structure_list;

#define structure_error (structure){.error=true};

typedef struct {
	declaration_list globals;
	structure_list structs;
	function_list functions;
} program;

// Error synchronisation
typedef bool (*synchronise_token_fn)(token t);

bool synchronise_token_struct(token t) {
	return t.type == token_none ||
		t.type == token_left_curly || 
		t.type == token_right_curly || 
		(t.type == token_keyword && (t.keyword == keyword_struct));
}

bool synchronise_token_statement(token t) {
	return  t.type == token_none ||
		t.type == token_semicolon || 
		t.type == token_left_curly || 
		t.type == token_right_curly || 
		(t.type == token_keyword && (t.keyword == keyword_if || t.keyword == keyword_else || t.keyword == keyword_for || t.keyword == keyword_while ));
}

bool synchronise_token_func(token t) {
	return t.type == token_none || t.type == token_right_curly;
}

void synchronise(lexer* lex, synchronise_token_fn synchronise_token) {
	token t = lexer_next_token(lex);
	while(!synchronise_token(t)) {
		t = lexer_next_token(lex);
	}
}

// printers

string keyword_to_string(keyword kw) {
    switch (kw) {
    case keyword_none: return sv("");
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
    case keyword_string: return sv("string");
    case keyword_error: return sv("error");
    case keyword_type: return sv("type");
    case keyword_var: return sv("var");
    case keyword_static: return sv("static");
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
	case primitive_none: return sv("");
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
	case primitive_string: return sv("string");
	}
}

string wrapped_to_string(wrapped_type_type typ) {
	switch(typ) {
	case wrapped_type_constant: return sv("const ");
	case wrapped_type_pointer: return sv("*");
	case wrapped_type_optional: return sv("?");
	case wrapped_type_result: return sv("!");
	}
}

string type_to_string(type typ) {
	switch(typ.type) {
	case type_none: return sv("");
	case type_primitive: return primitive_to_string(typ.as.primitive);
	case type_wrapped: return tconcat(wrapped_to_string(typ.as.wrapped.type), type_to_string(*typ.as.wrapped.inner));
	case type_slice: return tsprintf("[]%.*s", string_arg(type_to_string(*typ.as.slice.inner)));
	case type_array: return tsprintf("[%d]%.*s",typ.as.array.size, string_arg(type_to_string(*typ.as.array.inner)));
	case type_struct: {
		string s = tconcat(typ.as.structure.identifier, sv("}"));
		for (int i=0; i<typ.as.structure.parameters.len; i++) {
			s = tconcat(s, type_to_string(typ.as.structure.parameters.ptr[i]));
			if (i < typ.as.structure.parameters.len-1) {
				s = tconcat(s, sv(","));
			}
		}
		s = tconcat(s, sv("}"));
		return s;
	};
	case type_function: {
		string s = sv("func(");
		for (int i=0; i<typ.as.function.arguments.len; i++) {
			s = tconcat(s, type_to_string(typ.as.function.arguments.ptr[i]));
			if (i < typ.as.function.arguments.len-1) {
				s = tconcat(s, sv(","));
			}
		}
		s = tconcat(s, sv(")"));
		s = tconcat(s, type_to_string(*typ.as.function.return_type));
		return s;
	}
	}
}

string operator_to_string(operator op) {
	switch(op) {
	case operator_none: return sv("");
	case operator_plus: return sv("+");
	case operator_minus: return sv("-");
	case operator_star: return sv("*");
	case operator_slash: return sv("/");
	case operator_dot: return sv(".");
	case operator_ampersand: return sv("&");
	case operator_question: return sv("?");
	case operator_exclaimation: return sv("!");
	case operator_index: return sv("[");
	}
}

string expression_to_string(expression expr) {
	switch(expr.type) {
	case expression_type_none: return sv("");
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_string: return expr.as.literal.as.string;
		case expression_literal_char: return tsprintf("%c", expr.as.literal.as.character);
		case expression_literal_integer: return tsprintf("%lld", expr.as.literal.as.integer);
		}
	}
	case expression_type_func_call: {
		string s = sv("(");
		s = tconcat(s, expr.as.func_call.identifier);
		for (int i=0; i<expr.as.func_call.expressions.len; i++) {
			s = tconcat(s, sv(" "));
			s = tconcat(s, expression_to_string(expr.as.func_call.expressions.ptr[i]));
		}
		s = tconcat(s, sv(")"));
		return s;
	}
	case expression_type_identifier: return expr.as.identifier.identifier;
	case expression_type_tree: {
		string s = sv("(");
		s = tconcat(s, operator_to_string(expr.as.tree.op));
		for (int i=0; i<expr.as.tree.operands.len; i++) {
			s = tconcat(s, sv(" "));
			s = tconcat(s, expression_to_string(expr.as.tree.operands.ptr[i]));
		}
		s = tconcat(s, sv(")"));
		return s;
		return s;
	}
	}
}

// Parsers

bool token_not_empty_or_equals(lexer* lex, token_type typ) {
	token t = lexer_peek_token(lex);
	if (t.type != token_none && t.type != typ) return true;
	return false;
}

// primitive = int,float,...
primitive_type parse_primitive(keyword kw) {
	switch(kw) {
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
	case keyword_string: return primitive_string;
	default: return primitive_none;
	}
}

// type = const (*)(|?|!|[n]|[])(int|indef) | func(type...)type
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
		t = lexer_next_token(lex);
		if (t.type == token_integer) {
			typ.type = type_array;
			typ.as.array.size = string_to_number(t.value);

			t = lexer_next_token(lex);
			if (t.type != token_right_bracket) {
				report_parser_error(lex, tconcat(sv("expected ] but got"), t.value));
				return type_error;
			}
			typ.as.array.inner = malloc(sizeof(type));
			*(typ.as.array.inner) = parse_type(lex);
		} else {
			typ.type = type_slice;
			if (t.type != token_right_bracket) {
				report_parser_error(lex, tconcat(sv("expected ] but got"), t.value));
				return type_error;
			}
			typ.as.slice.inner = malloc(sizeof(type));
			*(typ.as.slice.inner) = parse_type(lex);
		}
	} else if (t.type == token_keyword) {
		if (t.keyword == keyword_const) {
			typ.type = type_wrapped;
			typ.as.wrapped.type = wrapped_type_constant;
			typ.as.wrapped.inner = malloc(sizeof(type));
			*(typ.as.wrapped.inner) = parse_type(lex);
		} else {
			typ.type = type_primitive;
			primitive_type primitive = parse_primitive(t.keyword);
			if (primitive == primitive_none) {
				report_parser_error(lex, tconcat(sv("expected type but got "), t.value));
				return type_error;
			}
			typ.as.primitive = primitive;
		}
	} else if (t.type == token_identifier) {
		typ.type = type_struct;
		typ.as.structure.identifier = t.value;
		todo("look up parameters for struct"); 
	} else if (t.type == token_keyword && t.keyword == keyword_func) {
		typ.type = type_function;
		t = lexer_next_token(lex);
		if (t.type != token_left_paren) {
			report_parser_error(lex, tconcat(sv("expected ( but got"), t.value));
			return type_error;
		}

		while((t = lexer_peek_token(lex)).type != token_right_paren) {
			type arg_type = parse_type(lex);
			array_append(&typ.as.function.arguments, arg_type);

			t = lexer_peek_token(lex);
			if (t.type == token_comma) lexer_next_token(lex);
			else if (t.type == token_right_paren) break;
			else {
				report_parser_error(lex, tconcat(sv("expected ) but got"), t.value));
				return type_error;
			}
		}
		lexer_next_token(lex);

		// TODO:Make return type optional
		typ.as.function.return_type = malloc(sizeof(type));
		*(typ.as.function.return_type) = parse_type(lex);
	} else {
		report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
		return type_error;
	}

	return typ;
}

// decl = type ident
binding parse_binding(lexer* lex) {
	binding bind = {0};
	bind.type = parse_type(lex);
	token t = lexer_next_token(lex);
	if (t.type != token_identifier) {
		report_parser_error(lex, tconcat(sv("expected identifier but got "), t.value));
		synchronise(lex, synchronise_token_statement);
	} else {
		bind.identifier = t.value;
	}
	return bind;
}

typedef struct {
	uint8_t left;
	uint8_t right;
} binding_power;

binding_power infix_binding_power(operator op) {
	static_assert(operator_none == 9, "infix_binding_power needs updating");
	switch(op) {
	case operator_plus:
	case operator_minus: return (binding_power){1, 2};
	case operator_star:
	case operator_slash: return (binding_power){3, 4};
	case operator_dot: return (binding_power){10, 9};
	default: unreachable;
	}
}

binding_power prefix_binding_power(operator op) {
	static_assert(operator_none == 9, "prefix_binding_power needs updating");
	switch(op) {
	case operator_plus:
	case operator_minus: return (binding_power){0, 5};
	case operator_ampersand:
	case operator_star: return (binding_power){0, 6};
	default: unreachable;
	}
}

binding_power postfix_binding_power(operator op) {
	static_assert(operator_none == 9, "postfix_binding_power needs updating");
	switch(op) {
	case operator_index:
	case operator_question:
	case operator_exclaimation: return (binding_power){8, 0};
	default: unreachable;
	}
}

expression parse_expression(lexer* lex);

// Literal and expression parsing
expression parse_expression_atom(lexer* lex) {
	expression ex = {0};
	token t = lexer_next_token(lex);
	switch(t.type) {
	case token_integer: {
		ex.type = expression_type_literal;
		ex.as.literal.type = expression_literal_integer;
		ex.as.literal.as.integer = lexer_value_to_integer(t.value);
	} break;
	case token_string: {
		ex.type = expression_type_literal;
		ex.as.literal.type = expression_literal_string;
		ex.as.literal.as.string = lexer_value_to_string(t.value);
	} break;
	case token_char: {
		ex.type = expression_type_literal;
		ex.as.literal.type = expression_literal_char;
		ex.as.literal.as.character = lexer_value_to_char(t.value);
	} break;
	case token_identifier: {
		if (lexer_peek_token(lex).type == token_left_paren) {
			ex.type = expression_type_func_call;
			ex.as.func_call.identifier = t.value;

			lexer_next_token(lex);
			while(token_not_empty_or_equals(lex, token_right_paren)) {
    			expression expr = parse_expression(lex);
				if (expr.type == expression_type_none) {
					return expression_error;
				}
    			array_append(&(ex.as.func_call.expressions), expr);
    
				t = lexer_peek_token(lex);
    			if (t.type == token_comma) lexer_next_token(lex);
    			else if (t.type == token_right_paren) break;
    			else {
        			report_parser_error(lex, tconcat(sv("expected ',' or ')' in function call got "), t.value));
        			synchronise(lex, synchronise_token_statement);
        			return expression_error;
    			}
			}
			lexer_next_token(lex);
		} else {
			ex.type = expression_type_identifier;
			ex.as.identifier.identifier = t.value;
		}
	} break;
	default: {
		report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
		synchronise(lex, synchronise_token_statement);
		return expression_error;
	}
	}
	return ex;
}

inline static bool is_infix_op(operator op) {
	static_assert(operator_none == 9, "is_infix_op needs updating");
	return op == operator_plus || op == operator_minus || op == operator_star 
	|| op == operator_slash || op == operator_dot;
}

inline static bool is_prefix_op(operator op) {
	static_assert(operator_none == 9, "is_prefix_op needs updating");
	return op == operator_plus || op == operator_minus || op == operator_star || op == operator_ampersand;
}

inline static bool is_postfix_op(operator op) {
	static_assert(operator_none == 9, "is_postfix_op needs updating");
	return op == operator_question || op == operator_exclaimation || op == operator_index;
}

operator parse_operator(token_type typ) {
	static_assert(operator_none == 9, "parse_operator needs updating");
    switch (typ) {
    case token_plus: return operator_plus;
    case token_minus: return operator_minus;
    case token_star: return operator_star; 
    case token_slash: return operator_slash;
    case token_dot: return operator_dot;
    case token_ampersand: return operator_ampersand;
    case token_question: return operator_question;
    case token_exclaimation: return operator_exclaimation;
    case token_left_bracket: return operator_index;
    default: return operator_none;
    }
}

// Pratt parsing
expression parse_expression_bp(lexer* lex, int min_bp) {
	expression lhs;

	token t = lexer_peek_token(lex);
	if (t.type == token_left_paren) {
		lexer_next_token(lex);
		lhs = parse_expression_bp(lex, 0);
		t = lexer_next_token(lex);
		if (t.type != token_right_paren) {
			report_parser_error(lex, tconcat(sv("expected ) but got "), t.value));
			synchronise(lex, synchronise_token_statement);
			return expression_error;
		}
	} else {
		operator op = parse_operator(t.type);
		if (is_prefix_op(op)) { // prefix operator
			lexer_next_token(lex);

			binding_power bp = prefix_binding_power(op);
			expression rhs = parse_expression_bp(lex, bp.right);
			if (rhs.type == expression_type_none) {
				return expression_error;
			}
			
			expression s = {0};
			s.type = expression_type_tree;
			s.as.tree.op = op;
			
			array_append(&s.as.tree.operands, rhs);

			lhs = s;
		} else if (op != operator_none) {
			report_parser_error(lex, tconcat(sv("unknown prefix operator "), t.value));
			synchronise(lex, synchronise_token_statement);
			return expression_error;
		} else {
			lhs = parse_expression_atom(lex);
		}

		while(true) {
			t = lexer_peek_token(lex);

			operator op = parse_operator(t.type);
			if (op == operator_none) break;

			expression s = {0};
			s.type = expression_type_tree;
			s.as.tree.op = op;

			if (is_postfix_op(op)) {
				binding_power bp = postfix_binding_power(op);
				if (bp.left < min_bp) break;
				lexer_next_token(lex);

				if (op == operator_index) {
					expression rhs = parse_expression_bp(lex, 0);
					if (rhs.type == expression_type_none) {
						return expression_error;
					}

					array_append(&s.as.tree.operands, lhs);
					array_append(&s.as.tree.operands, rhs);

					t = lexer_next_token(lex);
					if (t.type != token_right_bracket) {
						report_parser_error(lex, tconcat(sv("expected ] but got "), t.value));
						synchronise(lex, synchronise_token_statement);
						return expression_error;
					}
				} else {
					array_append(&s.as.tree.operands, lhs);
				}
			} else {
				binding_power bp = infix_binding_power(op);
				if (bp.left < min_bp) break;
				lexer_next_token(lex);

				expression rhs = parse_expression_bp(lex, bp.right);
				if (rhs.type == expression_type_none) {
					return expression_error;
				}

				array_append(&s.as.tree.operands, lhs);
				array_append(&s.as.tree.operands, rhs);
			}

			lhs = s;
		}
	}

	return lhs;
}

expression parse_expression(lexer* lex) {
	return parse_expression_bp(lex, 0);
}

declaration parse_declaration(lexer* lex) {
	token t = lexer_peek_token(lex);
	if (t.keyword == keyword_var) lexer_next_token(lex);
	declaration decl = {0};
	decl.loc = lexer_current_loc(lex);

	binding binding = parse_binding(lex);
	decl.type = binding.type;
	decl.identifier = binding.identifier;

	t = lexer_peek_token(lex);
	if (t.type == token_equal) {
		lexer_next_token(lex);
		expression expr = parse_expression(lex);
		if (expr.type == expression_type_none) return declaration_error;
		decl.value = expr;

		t = lexer_next_token(lex);
		if (t.type != token_semicolon) {
			report_parser_error(lex, tconcat(sv("expected ; but got "), t.value));
 			synchronise(lex, synchronise_token_statement);
 			return declaration_error;
		}
	} else if (t.type == token_semicolon) {
		lexer_next_token(lex);
		decl.value.type = expression_type_none;
	} else {
		report_parser_error(lex, tconcat(sv("expected end of declaration but got "), t.value));
		synchronise(lex, synchronise_token_statement);
	}

	return decl;
}

statement parse_statement(lexer* lex) {
	static_assert(statement_type_none == 7, "parse_statement needs updating");

	statement s = {0};
	s.loc = lexer_current_loc(lex);

	token t = lexer_peek_token(lex);
	if (t.type == token_keyword && t.keyword == keyword_return) {
		lexer_next_token(lex);
		s.type = statement_type_return;
		expression expr = parse_expression(lex);
		if (expr.type == expression_type_none) {
			return statement_error;
		}
		s.as.ret.value = expr;
	} else if (t.type == token_keyword && t.keyword == keyword_if) {
		lexer_next_token(lex);
		s.type = statement_type_if;
		expression expr = parse_expression(lex);
		if (expr.type == expression_type_none) {
			return statement_error;
		}
		s.as.iff.condition = expr;
		t = lexer_peek_token(lex);
		if (t.type == token_left_curly) {
			lexer_next_token(lex);
			while(token_not_empty_or_equals(lex, token_right_curly)) {
				statement st = parse_statement(lex);
				if (st.type != statement_type_none) {
					array_append(&(s.as.iff.body.statements), st);
				}
			}
			lexer_next_token(lex);
		} else {
			statement st = parse_statement(lex);
			if (st.type != statement_type_none) {
				array_append(&(s.as.iff.body.statements), st);
			}
		}
	} else if (t.type == token_keyword && t.keyword == keyword_while) {
		lexer_next_token(lex);
		s.type = statement_type_while;
		expression expr = parse_expression(lex);
		if (expr.type == expression_type_none) {
			return statement_error;
		}
		s.as.whil.condition = expr;
		t = lexer_peek_token(lex);
		if (t.type == token_left_curly) {
			lexer_next_token(lex);
			while(token_not_empty_or_equals(lex, token_right_curly)) {
				statement st = parse_statement(lex);
				if (st.type != statement_type_none) {
					array_append(&(s.as.whil.body.statements), st);
				}
			}
			lexer_next_token(lex);
		} else {
			statement st = parse_statement(lex);
			if (st.type != statement_type_none) {
				array_append(&(s.as.whil.body.statements), st);
			}
		}
	} else if (t.type == token_keyword && (t.keyword == keyword_var || t.keyword == keyword_const)) {
		if (t.keyword == keyword_var) lexer_next_token(lex);
		s.type = statement_type_decl;

		declaration decl = parse_declaration(lex);
		if (decl.error) {
			return statement_error;
		}
		s.as.declaration = decl;
	} else if (t.type == token_identifier) {
		lexer_next_token(lex);
		string identifier = t.value;

		t = lexer_peek_token(lex);
		if (t.type == token_equal) {
			lexer_next_token(lex);
			s.type = statement_type_assign;
			s.as.assignment.destination.type = destination_type_variable;
			s.as.assignment.destination.identifier = identifier;
			expression expr = parse_expression(lex);
			if (expr.type == expression_type_none) {
				return statement_error;
			}
			s.as.assignment.value = expr;
		} else if (t.type == token_left_bracket) {
			lexer_next_token(lex);
			s.type = statement_type_assign;
			s.as.assignment.destination.type = destination_type_indexed;
			s.as.assignment.destination.identifier = identifier;
			expression expr = parse_expression(lex);
			if (expr.type == expression_type_none) {
				return statement_error;
			}
			s.as.assignment.destination.as.indexed.value = expr;
			t = lexer_next_token(lex);
			if (t.type != token_right_bracket) {
				report_parser_error(lex, tconcat(sv("expected ] got "), t.value));
        		synchronise(lex, synchronise_token_statement);
        		return statement_error;
			}
			t = lexer_next_token(lex);
			if (t.type != token_equal) {
				report_parser_error(lex, tconcat(sv("expected = got "), t.value));
        		synchronise(lex, synchronise_token_statement);
        		return statement_error;
			}

			expr = parse_expression(lex);
			if (expr.type == expression_type_none) {
				return statement_error;
			}
			s.as.assignment.value = expr;
		} else if (t.type == token_left_paren) {
			lexer_next_token(lex);
			
			s.type = statement_type_func_call;
			s.as.func_call.identifier = identifier;
			while(token_not_empty_or_equals(lex, token_right_paren)) {
    			expression expr = parse_expression(lex);
				if (expr.type == expression_type_none) {
					return statement_error;
				}
    			array_append(&(s.as.func_call.expressions), expr);
    
				t = lexer_peek_token(lex);
    			if (t.type == token_comma) lexer_next_token(lex);
    			else if (t.type == token_right_paren) break;
    			else {
        			report_parser_error(lex, tconcat(sv("expected ',' or ')' in function call got"), t.value));
        			synchronise(lex, synchronise_token_statement);
        			return statement_error;
    			}
			}
			lexer_next_token(lex);
		} else {
			report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
        	synchronise(lex, synchronise_token_statement);
			return statement_error;
		}
	} else if (t.type == token_left_curly) {
		lexer_next_token(lex);
		s.type = statement_type_scope;

		while(token_not_empty_or_equals(lex, token_right_curly)) {
			statement st = parse_statement(lex);
			if (st.type != statement_type_none) {
				array_append(&(s.as.scope.statements), st);
			}
		}
		lexer_next_token(lex);
	} else {
		report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
		synchronise(lex, synchronise_token_statement);
		return statement_error;
	}

	// consuming semicolon
	if (lexer_peek_token(lex).type == token_semicolon) lexer_next_token(lex);

	return s;
}

structure parse_struct(lexer* lex) {
	structure s = {0};
	s.loc = lexer_current_loc(lex);

	// Parsing identifier
	token t = lexer_next_token(lex);
	if (t.type != token_identifier) {
		report_parser_error(lex, tconcat(sv("expected identifier but got "), t.value));
	} else {
		s.identifier = t.value;
	}

	t = lexer_next_token(lex);
	if (t.type == token_less_than) {
		todo("generic parsing in structs");
	}

	if (t.type == token_left_curly) {
		while(token_not_empty_or_equals(lex, token_right_curly)) {
			binding bind = parse_binding(lex);
 			array_append(&s.parameters, bind);

 			t = lexer_peek_token(lex);
 			if (t.type == token_comma) lexer_next_token(lex);
 			else if (t.type == token_right_curly) break;
 			else {
 				report_parser_error(lex, tconcat(sv("expected } but got "), t.value));
 				synchronise(lex, synchronise_token_struct);
 				return structure_error;
 			}
		}
		lexer_next_token(lex);
	} else {
		report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
 		synchronise(lex, synchronise_token_struct);
		return structure_error;
	}

	return s;
}

function parse_function(lexer* lex) {
	function func = {0};
	func.loc = lexer_current_loc(lex);

	token t = lexer_next_token(lex);
	if (t.type != token_identifier) {
		report_parser_error(lex, tconcat(sv("expected identifier but got "), t.value));
	} else {
		func.identifier = t.value;
	}

	t = lexer_next_token(lex);
	if (t.type == token_less_than) {
		todo("generic parsing in structs");
	}

	if (t.type == token_left_paren) {
		while(token_not_empty_or_equals(lex, token_right_paren)) {
			binding bind = parse_binding(lex);
 			array_append(&func.arguments, bind);

 			t = lexer_peek_token(lex);
 			if (t.type == token_comma) lexer_next_token(lex);
 			else if (t.type == token_right_paren) break;
 			else {
 				report_parser_error(lex, tconcat(sv("expected ) but got "), t.value));
 				synchronise(lex, synchronise_token_func);
 				return function_error;
 			}
		}
		lexer_next_token(lex);
	} else {
		report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
 		synchronise(lex, synchronise_token_func);
		return function_error;
	}

	t = lexer_peek_token(lex);
	if (t.type != token_left_curly) {
		type type = parse_type(lex);
		if (type.type == type_none) return function_error;
		func.return_type = type;
	} else {
		func.return_type.type = type_primitive;
		func.return_type.as.primitive = primitive_void;
	}

	t = lexer_next_token(lex);
	if (t.type != token_left_curly) {
		report_parser_error(lex, tconcat(sv("expected { but got "), t.value));
		synchronise(lex, synchronise_token_func);
		return function_error;
	}

	while(token_not_empty_or_equals(lex, token_right_curly)) {
		statement st = parse_statement(lex);
		if(st.type != statement_type_none) array_append(&(func.body.statements), st);
	}
	lexer_next_token(lex);

	return func;
}

program parse_program(lexer* lex) {
	program prg = {0};

	token t = lexer_next_token(lex);
	while (t.type != token_none) {
		if (t.type == token_keyword && t.keyword == keyword_struct) {
			structure strukt = parse_struct(lex);
			if (!strukt.error) array_append(&prg.structs, strukt);
		} else if (t.type == token_keyword && t.keyword == keyword_func) {
			function func = parse_function(lex);
			if (!func.error) array_append(&prg.functions, func);
		} else if (t.type == token_keyword && (t.keyword == keyword_var || t.keyword == keyword_const)) {
			declaration decl = parse_declaration(lex);
			if (!decl.error) array_append(&prg.globals, decl);
		} else {
			report_parser_error(lex, tsprintf("encountered unknown token `%.*s` at top level", string_arg(t.value)));
			synchronise(lex, synchronise_token_func);
		}
		t = lexer_next_token(lex);
	}

	return prg;
}