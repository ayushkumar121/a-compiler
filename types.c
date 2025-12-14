typedef struct type type;

typedef struct {
	int len;
	int cap;
	type* ptr;
} type_list;

typedef enum {
	type_none,
	type_primitive,
	type_wrapped,
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
	wrapped_type_slice,
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
		struct_type structure;
		function_type function;
	} as;
} type;

#define type_error (struct type){.type=type_none}
