// begin types

typedef struct type type;
typedef struct expression expression;
typedef struct statement statement;
typedef struct statement_list statement_list;
typedef struct scope scope;
typedef struct binding binding;
typedef struct binding_list binding_list;
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
	// TODO: slice?
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
	bool complete;
	string identifier;
	int field_count;
	string* field_names;
	type* field_types;
} struct_type;

typedef struct {
	string identifier;
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

#define type_error (struct type){.type=type_none}

// end types

typedef struct binding {
	bool error;
	type type;
	string identifier;
} binding;

#define binding_error (binding){.error=true}

typedef struct binding_list {
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

typedef struct expression_list {
	int len;
	int cap;
	expression *ptr;
} expression_list;

typedef struct {
	string identifier;
	expression_list expressions;
} func_call;

typedef struct {
	operator op;
	expression_list operands;
} expression_tree;

typedef struct expression {
	expression_type type;
	union {
		expression_literal literal;
		string identifier;
		expression_tree tree;
		func_call func_call;
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
	statement_type_list,
	statement_type_return,
	statement_type_if,
	statement_type_while,
	statement_type_none,
} statement_type;

typedef enum {
	lvalue_type_variable,
	lvalue_type_indexed,
	lvalue_type_indirect,
	lvalue_type_field,
} lvalue_type;

typedef struct {
	lvalue_type type;
	string identifier;
	union {
		expression index;
		string field;
	} as;
} lvalue;

typedef struct statement_list {
	int len;
	int cap;
	statement* ptr;
} statement_list;

typedef struct {
	lvalue lvalue;
	expression value;
} statement_assign;

typedef struct {
	expression value;
} statement_return;

typedef struct {
	expression condition;
	statement_list iff_body;
	statement_list else_body;
} statement_if;

typedef struct {
	expression condition;
	statement_list body;
} statement_while;

typedef struct statement {
	statement_type type;
	union {
		declaration declaration;
		statement_assign assignment;
		func_call func_call;
		statement_list list;
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
	statement_list body;
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
    unreachable();
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
	unreachable();
}

string wrapped_to_string(wrapped_type_type typ) {
	switch(typ) {
	case wrapped_type_constant: return sv("const ");
	case wrapped_type_pointer: return sv("*");
	case wrapped_type_optional: return sv("?");
	case wrapped_type_result: return sv("!");
	}
	unreachable();
}

string type_to_string(type typ) {
	switch(typ.type) {
	case type_none: return sv("");
	case type_primitive: return primitive_to_string(typ.as.primitive);
	case type_wrapped: return tconcat(wrapped_to_string(typ.as.wrapped.type), type_to_string(*typ.as.wrapped.inner));
	case type_slice: return tsprintf("[]%.*s", sarg(type_to_string(*typ.as.slice.inner)));
	case type_array: return tsprintf("[%d]%.*s",typ.as.array.size, sarg(type_to_string(*typ.as.array.inner)));
	case type_struct: {
		string s = tconcat(typ.as.structure.identifier, sv("{"));
		if (typ.as.structure.complete) {
			for (int i=0; i<typ.as.structure.field_count; i++) {
				s = tconcat(s, typ.as.structure.field_names[i]);
				s = tconcat(s, sv(":"));
				s = tconcat(s, type_to_string(typ.as.structure.field_types[i]));
				if (i < typ.as.structure.field_count-1) {
					s = tconcat(s, sv(", "));
				}
			}
		} else {
			s = tconcat(s, sv("..."));
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
	unreachable();
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
	case operator_index: return sv("[]");
	}
	unreachable();
}

string expression_to_string(expression expr) {
	switch(expr.type) {
	case expression_type_none: return sv("");
	case expression_type_literal: {
		switch(expr.as.literal.type) {
		case expression_literal_string: return expr.as.literal.as.string;
		case expression_literal_char: return tsprintf("%c", expr.as.literal.as.character);
		case expression_literal_integer: return tsprintf("%ld", (long)expr.as.literal.as.integer);
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
	case expression_type_identifier: return expr.as.identifier;
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
	unreachable();
}

// type helpers

type type_of_primitive(primitive_type p) {
	type typ = {0};
	typ.type = type_primitive;
	typ.as.primitive = p;
	return typ;
}

type type_of_struct(structure s) {
	type typ = {0};
	typ.type = type_struct;
	typ.as.structure.identifier = s.identifier;
	typ.as.structure.complete = true;
	typ.as.structure.field_count = s.parameters.len;
	typ.as.structure.field_names = malloc(typ.as.structure.field_count*sizeof(string));
	typ.as.structure.field_types = malloc(typ.as.structure.field_count*sizeof(type));
	for (int i=0; i<typ.as.structure.field_count; i++) {
		typ.as.structure.field_names[i] = s.parameters.ptr[i].identifier;
		typ.as.structure.field_types[i] = s.parameters.ptr[i].type;
	}
	return typ;
}

type type_of_function(function fn) {
	type typ = {0};
	typ.type = type_function;
	typ.as.function.return_type = malloc(sizeof(type));
	typ.as.function.identifier = fn.identifier;
	*(typ.as.function.return_type) = fn.return_type;
	for (int i=0; i<fn.arguments.len; i++) {
		array_append(&typ.as.function.arguments, fn.arguments.ptr[i].type);
	}
	return typ;
}

type type_of_wrapped(wrapped_type_type wrapped_type, type* inner) {
	type typ = {0};
	typ.type = type_wrapped;
	typ.as.wrapped.type = wrapped_type;
	typ.as.wrapped.inner = inner;
	return typ;
}

void type_free(type* t) {
	ASSERT(t != NULL);
	switch(t->type) {
	case type_none:
	case type_primitive:
		break;
	case type_wrapped:
		type_free(t->as.wrapped.inner);
		break;
	case type_slice:
		type_free(t->as.slice.inner);
		break;
	case type_array:
		type_free(t->as.array.inner);
		break;
	case type_struct:
		if (t->as.structure.field_count>0) type_free(t->as.structure.field_types);
		break;
	case type_function:
		if (t->as.function.arguments.len>0) type_free(t->as.function.arguments.ptr);
		type_free(t->as.function.return_type);
		break;
	}
	free(t);
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
		typ.as.structure.complete = false;
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
	token t = lexer_next_token(lex);
	if (t.type != token_identifier) {
		report_parser_error(lex, tconcat(sv("expected identifier but got "), t.value));
		synchronise(lex, synchronise_token_statement);
		return binding_error;
	} else {
		bind.identifier = t.value;
	}
	type type = parse_type(lex);
	if (type.type == type_none) {
		return binding_error;
	}
	bind.type = type;
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
	default: unreachable();
	}
}

binding_power prefix_binding_power(operator op) {
	static_assert(operator_none == 9, "prefix_binding_power needs updating");
	switch(op) {
	case operator_plus:
	case operator_minus: return (binding_power){0, 5};
	case operator_ampersand:
	case operator_star: return (binding_power){0, 6};
	default: unreachable();
	}
}

binding_power postfix_binding_power(operator op) {
	static_assert(operator_none == 9, "postfix_binding_power needs updating");
	switch(op) {
	case operator_index:
	case operator_question:
	case operator_exclaimation: return (binding_power){8, 0};
	default: unreachable();
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
			ex.as.identifier = t.value;
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
	if (binding.error) {
		return declaration_error;
	}
	decl.type = binding.type;
	decl.identifier = binding.identifier;

	t = lexer_peek_token(lex);
	if (t.type == token_equal) {
		lexer_next_token(lex);
		expression expr = parse_expression(lex);
		if (expr.type == expression_type_none) return declaration_error;
		decl.value = expr;
	} else if (t.type == token_semicolon) {
		decl.value.type = expression_type_none;
	} else {
		report_parser_error(lex, tconcat(sv("expected end of declaration but got "), t.value));
		synchronise(lex, synchronise_token_statement);
	}

	return decl;
}

statement parse_statement(lexer* lex) {
	static_assert(statement_type_none == 7, "parse_statement needs updating");

	// consume semicolon as empty statements
	while (lexer_peek_token(lex).type == token_semicolon) lexer_next_token(lex);

	statement s = {0};
	s.loc = lexer_current_loc(lex);

	token t = lexer_peek_token(lex);
	if (t.type == token_keyword && t.keyword == keyword_return) {
		lexer_next_token(lex);
		s.type = statement_type_return;

		if (lexer_peek_token(lex).type == token_semicolon) {
			s.as.ret.value = expression_error;
		} else {
			expression expr = parse_expression(lex);
			if (expr.type == expression_type_none) {
				return statement_error;
			}
			s.as.ret.value = expr;
		}

		// expected ; after statement
		if (lexer_peek_token(lex).type == token_semicolon) lexer_next_token(lex);
		else {
			printf(sfmt"\n", sarg(lexer_current_value(lex)));
			report_parser_error(lex, sv("expected semicolon"));
			synchronise(lex, synchronise_token_statement);
			return statement_error;
		}
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
					array_append(&(s.as.iff.iff_body), st);
				}
			}
			lexer_next_token(lex);
		} else {
			statement st = parse_statement(lex);
			if (st.type != statement_type_none) {
				array_append(&(s.as.iff.iff_body), st);
			}
		}

		// Else block is optional
		t = lexer_peek_token(lex);
		printf("we have else token! "sfmt" %d %d\n", sarg(t.value), t.type, t.keyword);
		if (t.type == token_keyword && t.keyword == keyword_else) {
			lexer_next_token(lex);
			if (t.type == token_left_curly) {
				lexer_next_token(lex);
				while(token_not_empty_or_equals(lex, token_right_curly)) {
					statement st = parse_statement(lex);
					if (st.type != statement_type_none) {
						array_append(&(s.as.iff.else_body), st);
					}
				}
				lexer_next_token(lex);
			} else {
				statement st = parse_statement(lex);
				if (st.type != statement_type_none) {
					array_append(&(s.as.iff.else_body), st);
				}
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
					array_append(&(s.as.whil.body), st);
				}
			}
			lexer_next_token(lex);
		} else {
			statement st = parse_statement(lex);
			if (st.type != statement_type_none) {
				array_append(&(s.as.whil.body), st);
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

		// expected ; after statement
		if (lexer_peek_token(lex).type == token_semicolon) lexer_next_token(lex);
		else {
			printf(sfmt"\n", sarg(lexer_current_value(lex)));
			report_parser_error(lex, sv("expected semicolon"));
			synchronise(lex, synchronise_token_statement);
			return statement_error;
		}
	} else if (t.type == token_identifier) {
		lexer_next_token(lex);
		string identifier = t.value;

		t = lexer_peek_token(lex);
		if (t.type == token_equal) {
			lexer_next_token(lex);
			s.type = statement_type_assign;
			s.as.assignment.lvalue.type = lvalue_type_variable;
			s.as.assignment.lvalue.identifier = identifier;
			expression expr = parse_expression(lex);
			if (expr.type == expression_type_none) {
				return statement_error;
			}
			s.as.assignment.value = expr;
		} else if (t.type == token_left_bracket) {
			lexer_next_token(lex);
			s.type = statement_type_assign;
			s.as.assignment.lvalue.type = lvalue_type_indexed;
			s.as.assignment.lvalue.identifier = identifier;
			expression expr = parse_expression(lex);
			if (expr.type == expression_type_none) {
				return statement_error;
			}
			s.as.assignment.lvalue.as.index = expr;
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
		} else if (t.type == token_star) {
			lexer_next_token(lex);
			s.type = statement_type_assign;
			s.as.assignment.lvalue.type = lvalue_type_indirect;
			s.as.assignment.lvalue.identifier = identifier;
			t = lexer_next_token(lex);
			if (t.type != token_equal) {
				report_parser_error(lex, tconcat(sv("expected = got "), t.value));
				synchronise(lex, synchronise_token_statement);
				return statement_error;
			}
			expression expr = parse_expression(lex);
			if (expr.type == expression_type_none) {
				return statement_error;
			}
			s.as.assignment.value = expr;
		} else if (t.type == token_dot) {
			lexer_next_token(lex);
			s.type = statement_type_assign;
			s.as.assignment.lvalue.type = lvalue_type_field;
			s.as.assignment.lvalue.identifier = identifier;
			t = lexer_next_token(lex);
			if (t.type != token_identifier) {
				report_parser_error(lex, tconcat(sv("expected field name got "), t.value));
				synchronise(lex, synchronise_token_statement);
				return statement_error;
			}
			s.as.assignment.lvalue.as.field = t.value;

			t = lexer_next_token(lex);
			if (t.type != token_equal) {
				report_parser_error(lex, tconcat(sv("expected = got "), t.value));
				synchronise(lex, synchronise_token_statement);
				return statement_error;
			}
			expression expr = parse_expression(lex);
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

		// expected ; after statement
		if (lexer_peek_token(lex).type == token_semicolon) lexer_next_token(lex);
		else {
			printf(sfmt"\n", sarg(lexer_current_value(lex)));
			report_parser_error(lex, sv("expected semicolon"));
			synchronise(lex, synchronise_token_statement);
			return statement_error;
		}
	} else if (t.type == token_left_curly) {
		lexer_next_token(lex);
		s.type = statement_type_list;

		while(token_not_empty_or_equals(lex, token_right_curly)) {
			statement st = parse_statement(lex);
			if (st.type != statement_type_none) {
				array_append(&(s.as.list), st);
			}
		}
		lexer_next_token(lex);
	} else {
		report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
		synchronise(lex, synchronise_token_statement);
		return statement_error;
	}

	return s;
}

void statements_free(statement_list stms) {
	for (int i=0; i<stms.len; i++) {
		switch(stms.ptr[i].type) {
		case statement_type_list:
			statements_free(stms.ptr[i].as.list);
			break;
		case statement_type_func_call:
			free(stms.ptr[i].as.func_call.expressions.ptr);
			break;
		default: break;
		}
	}
	free(stms.ptr);
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
			binding binding = parse_binding(lex);
			if (binding.error) {
				return structure_error;
			}
 			array_append(&s.parameters, binding);

			// consuming semicolon
			if (lexer_peek_token(lex).type == token_semicolon) lexer_next_token(lex);
		}
		lexer_next_token(lex);
	} else {
		report_parser_error(lex, tconcat(sv("unexpected token "), t.value));
 		synchronise(lex, synchronise_token_struct);
		return structure_error;
	}

	return s;
}

void structs_free(structure_list structs) {
	free(structs.ptr);
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
			binding binding = parse_binding(lex);
			if (binding.error) {
				return function_error;
			}
 			array_append(&func.arguments, binding);

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
		if(st.type != statement_type_none) array_append(&(func.body), st);
	}
	lexer_next_token(lex);

	return func;
}

void functions_free(function_list funcs) {
	for (int i=0; i<funcs.len; i++) {
		free(funcs.ptr[i].arguments.ptr);
		statements_free(funcs.ptr[i].body);
	}
	free(funcs.ptr);
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
			report_parser_error(lex, tsprintf("encountered unknown token `%.*s` at top level", sarg(t.value)));
			synchronise(lex, synchronise_token_func);
		}
		t = lexer_next_token(lex);
	}

	return prg;
}
