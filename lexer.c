typedef enum {
	token_none,
	token_left_paren,
	token_right_paren,
	token_left_curly,
	token_right_curly,
	token_left_bracket,
	token_right_bracket,
	token_less_than,
	token_greater_than,
	token_plus,
	token_minus,
	token_star,
	token_equal,
	token_slash,
	token_dot,
	token_ampersand,
	token_comma,
	token_question,
	token_exclaimation,
	token_semicolon,
	token_keyword,
	token_identifier,
	token_integer,
	token_string,
	token_char,
} token_type;

typedef enum {
	keyword_void,
	keyword_int,
	keyword_uint,
	keyword_byte,
	keyword_ubyte,
	keyword_short,
	keyword_ushort,
	keyword_long,
	keyword_ulong,
	keyword_float,
	keyword_double,
	keyword_string,
	keyword_error,
	keyword_type,
	keyword_static,
	keyword_var,
	keyword_const,
	keyword_enum,
	keyword_if,
	keyword_else,
	keyword_for,
	keyword_foreach,
	keyword_while,
	keyword_switch,
	keyword_case,
	keyword_default,
	keyword_break,
	keyword_continue,
	keyword_return,
	keyword_extern,
	keyword_struct,
	keyword_func,
	keyword_union,
	keyword_import,
	keyword_nil,
	keyword_none,
} keyword;

typedef struct {
	token_type type;
	keyword keyword;
	string value;
} token;

#define token_eof (token){.type=token_none}

typedef struct {
	int start;
	int end;
	string source;
	string file;
} lexer;

inline static string lexer_current_value(lexer* lex) {
	return (string){lex->end - lex->start, lex->source.ptr + lex->start};
}

#define keyword_cmp(k, s) (k[0] == s.ptr[0] && sizeof(k)-1 == s.len && memcmp(k, s.ptr, s.len) == 0)

keyword lexer_match_keyword(string s) {
	static_assert (keyword_none == 35, "lexer_match_keyword needs updating");

	if (keyword_cmp("void", s)) {
		return keyword_void;
	} else if (keyword_cmp("int", s)) {
		return keyword_int;
	} else if (keyword_cmp("uint", s)) {
		return keyword_uint;
	} else if (keyword_cmp("byte", s)) {
		return keyword_byte;
	} else if (keyword_cmp("ubyte", s)) {
		return keyword_ubyte;
	} else if (keyword_cmp("short", s)) {
		return keyword_short;
	} else if (keyword_cmp("ushort", s)) {
		return keyword_ushort;
	} else if (keyword_cmp("long", s)) {
		return keyword_long;
	} else if (keyword_cmp("ulong", s)) {
		return keyword_ulong;
	} else if (keyword_cmp("float", s)) {
		return keyword_float;
	} else if (keyword_cmp("double", s)) {
		return keyword_double;
	} else if (keyword_cmp("string", s)) {
		return keyword_string;
	} else if (keyword_cmp("error", s)) {
		return keyword_error;
	} else if (keyword_cmp("type", s)) {
		return keyword_type;
	} else if (keyword_cmp("static", s)) {
		return keyword_static;
	} else if (keyword_cmp("var", s)) {
		return keyword_var;
	} else if (keyword_cmp("const", s)) {
		return keyword_const;
	} else if (keyword_cmp("enum", s)) {
		return keyword_enum;
	} else if (keyword_cmp("struct",s)) {
		return keyword_struct;
	} else if (keyword_cmp("func",s)) {
		return keyword_func;
	} else if (keyword_cmp("if", s)) {
		return keyword_if;
	} else if (keyword_cmp("else", s)) {
		return keyword_else;
	} else if (keyword_cmp("for", s)) {
		return keyword_for;
	} else if (keyword_cmp("foreach", s)) {
		return keyword_foreach;
	} else if (keyword_cmp("while", s)) {
		return keyword_while;
	} else if (keyword_cmp("switch", s)) {
		return keyword_switch;
	} else if (keyword_cmp("case", s)) {
		return keyword_case;
	} else if (keyword_cmp("default", s)) {
		return keyword_default;
	} else if (keyword_cmp("break", s)) {
		return keyword_break;
	} else if (keyword_cmp("continue", s)) {
		return keyword_continue;
	} else if (keyword_cmp("return", s)) {
		return keyword_return;
	} else if (keyword_cmp("extern", s)) {
		return keyword_extern;
	} else if (keyword_cmp("union", s)) {
		return keyword_union;
	} else if (keyword_cmp("import", s)) {
		return keyword_import;
	} else if (keyword_cmp("nil", s)) {
		return keyword_nil;
	}
	return keyword_none;
}

token_type lexer_match_symbols(char ch) {
	switch (ch) {
  case '(': return token_left_paren;
	case ')': return token_right_paren;	
  case '{': return token_left_curly;
	case '}': return token_right_curly;
	case '[': return token_left_bracket;
	case ']': return token_right_bracket;
	case '<': return token_less_than;
	case '>': return token_greater_than;
	case '+': return token_plus;
	case '-': return token_minus;
	case '*': return token_star;
	case '/': return token_slash;
	case '=': return token_equal;
	case '.': return token_dot;
	case ',': return token_comma;
	case '?': return token_question;
	case '!': return token_exclaimation;
	case ';': return token_semicolon;
	case '&': return token_ampersand;
	}
 	return 0;
}

#define lexer_from_string(source) lexer_from_string_(source, sv(__FILE__))

lexer lexer_from_string_(string source, string file_path) {
	lexer lex = {0};
	lex.file = file_path;
	lex.source = source;
	return lex;
}

lexer lexer_from_file(string file_path) {
	lexer lex = {0};
	lex.file = file_path;
	lex.source = file_read_to_string(file_path.ptr);
	return lex;
}

token lexer_peek_token(lexer* lex) {
	// Skip whitespace
	while (lex->start < lex->source.len && isspace(lex->source.ptr[lex->start])) {
	  lex->start++;
	}
	if (lex->start >= lex->source.len) {
	  return token_eof;
	}
	lex->end = lex->start;

	// Skip comments
	if (lex->start < lex->source.len-1 && lex->source.ptr[lex->start] == '/' &&  lex->source.ptr[lex->start+1] == '/') {
		while (lex->start < lex->source.len && lex->source.ptr[lex->start] != '\n') {
	  	lex->start++;
		}
		lex->start++; // skipping new line
		lex->end = lex->start;
	}

	// Matching Symbols
	token_type symbol = lexer_match_symbols(lex->source.ptr[lex->end]);
	if (symbol != token_none) {
		lex->end++;
		
		string value = lexer_current_value(lex);
		return (token){.type=symbol, .value=value};
	}

	// Matching identifier/keyword
	if (isalpha(lex->source.ptr[lex->start]) || lex->source.ptr[lex->start] == '_') {
		lex->end++;
		while (lex->end < lex->source.len && 
	       (isalnum(lex->source.ptr[lex->end]) || lex->source.ptr[lex->end] == '_')) {
	    	lex->end++;
		}

		string value = lexer_current_value(lex);
		keyword kw = lexer_match_keyword(value);

		if (kw != keyword_none) {
			return (token){.type=token_keyword, .keyword=kw, .value=value};
		} else {
			return (token){.type=token_identifier, .value=value};
		}
	}

	// Matching numbers
	if (isdigit(lex->source.ptr[lex->start])) {
		while (lex->end < lex->source.len && isdigit(lex->source.ptr[lex->end])) {
	    	lex->end++;
		}
		string value = lexer_current_value(lex);
		return (token){.type=token_integer, .value=value};
	}

	// Matching strings
	if (lex->source.ptr[lex->start] == '\"') {
		lex->end++;
		while (lex->end < lex->source.len && lex->source.ptr[lex->end] != '\"') {
	    	lex->end++;
		}
		if (lex->end >= lex->source.len) {
			return token_eof;
		}
		lex->end++;
		string value = lexer_current_value(lex);
		return (token){.type=token_string, .value=value};
	}


	// Matching chars
	if (lex->source.ptr[lex->start] == '\'') {
		lex->end++;
		if (lex->end < lex->source.len && lex->source.ptr[lex->end] == '\\') {
			lex->end++;
			if (lex->end < lex->source.len) {
				lex->end++;
			}
		} else if (lex->end < lex->source.len && lex->source.ptr[lex->end] != '\'') {
			lex->end++;
		}
		if (lex->end >= lex->source.len || lex->source.ptr[lex->end] != '\'') {
			return token_eof;
		}
		lex->end++;
		string value = lexer_current_value(lex);
		return (token){.type=token_char, .value=value};
	}

	return token_eof;
}

token lexer_next_token(lexer* lex) {
	token t = lexer_peek_token(lex);
	// println(t.value);
	lex->start = lex->end;
	return t;
}

typedef struct {
	string file;
	string source;
	int line;
	int col;
} lexer_file_loc;

lexer_file_loc lexer_current_loc(lexer* lex) {
	int line = 1;
	int col = 1;
	int i = 0;
	while (i < lex->start) {
		if (lex->source.ptr[i] == '\n') {
			col = 1;
			line++;
		} else {
			col++;
		}
		i++;
	}
	return (lexer_file_loc){lex->file, lex->source, line, col};
}

int lexer_source_line_count(string source) {
	int lines = 1;
	int i = 0;
	while (i < source.len) {
		if (source.ptr[i] == '\n') {
			lines++;
		}
		i++;
	}
	return lines;
}

string lexer_source_line(string source, int line) {
	int lines = 1;
	int i = 0;
	while (i < source.len) {
		if (lines == line) break;
		if (source.ptr[i] == '\n') {
			lines++;
		}
		i++;
	}
	int line_start = i;
	int line_end = line_start;
	while (line_end < source.len && source.ptr[line_end] != '\n') {
		line_end++;
	}
	return (string){line_end-line_start, source.ptr+line_start};
}

string unquote(string s) {
  return (string){s.len-2, s.ptr+1};
}

char lexer_value_to_char(string lex_value) {
 	return character_escape(unquote(lex_value));
}

string lexer_value_to_string(string lex_value) {
	string s = unquote(lex_value);
	string_builder sb = {0};
	int i = 0;
	while(i<s.len) {
		if (s.ptr[i] != '\\') {
			array_append(&sb, s.ptr[i]);
			i += 1;
		} else {
			string ss = {2, s.ptr+i};
			char ch = character_escape(ss);
			array_append(&sb, ch);
			i += 2;
		}
	}
 	return (string){sb.len, sb.ptr};
}

int64_t lexer_value_to_integer(string lex_value) {
  	return string_to_number(lex_value);
}

// Error reporting

#define report_parser_error(lex, message) repor_error_(lexer_current_loc(lex), message, __FILE__, __LINE__)
#define report_compiler_error(loc, message) repor_error_(loc, message, __FILE__, __LINE__)

void repor_error_(lexer_file_loc loc, string message, const char* file, int line) {
	fprintf(stderr, "%.*s:%d:%d error: %.*s see compiler:%s:%d\n", 
		sarg(loc.file), loc.line, loc.col, sarg(message), file, line);

	int line_count = lexer_source_line_count(loc.source);
	for (int i = max(loc.line-2, 1); i<=min(loc.line+2, line_count); i++) {
		string source_line = lexer_source_line(loc.source, i);
		if (i == loc.line) fprintf(stderr, "->");
		else fprintf(stderr, "  ");
		fprintf(stderr, "%5d|%.*s\n", i, sarg(source_line));
	}

	error_count++;
}

