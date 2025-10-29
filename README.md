# A small language like c but simpler
===

## Goals:
- Simple Languge (Easy to use & Easy to reason)
- Generics
- Optional and Result type
- Language support for common idioms
- Module system

## References:
- https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
- 

## Examples:

1. Optional types
```c++
?int divide(int n, int d) {
	if (d == 0) return nil;
	return n/d;
}
```

2. Result types
```c++
!int divide(int n, int d) {
	if (d == 0) return error("cannot devide by zero");
	return n/d;
}

int! main() {
	int! result = divide(10, 2)
	if (result) {
		printf("foo = %v", result!);
	}
}
```

3. Json Decoding
```c++
import json;

json_value! main() {
	json_value! result = json.decode_from_string("{\"foo\":2}");
	if (result) {
		printf("foo = %v", result!["foo"]);
	}
}
```

4. Generics
```c++
import json;

void my_memset<type T>(T[] arr, T value) {
	for (size_t i = 0; i<arr.len; i++) {
		arr[i] = value;
	}
}

void main() {
	int[10] arr;
	my_memset(arr, 10); 
}
```

5. RAII (More research is needed)

```c++
struct buffer {
	byte[] data
}

void delete(buffer b) { // Called on scoped exit
	println("buffer deleted");
	delete(b.data);
}

void main() {
	buffer b; 
	b.data = new(10);
}
```

### Memory model
- Stack values: owned by the scope, deleted at scope exit
- Heap pointers: manually managed with delete()
- Pass by value: moves by default (use & for borrowing)
- Return by value: moves ownership to caller

6. C FFI
```c++
import json;

// External library
extern "InitWindow" func init_window(int32 width, int32 height char* window_name);

// Exposed externally
extern json_value! nc json_parse(char* json) {
	return json.decode_from_string(string_from_cstr(json));
}
```

7. Iterators
```c++
struct reverse_iter<type T> {
    T[] data;
    size_t index;
}

T? next<T>(reverse_iter<T>* iter) {
    if (iter.index > 0) {
        iter.index--;
        return iter.data[iter.index];
    }
    return nil;
}

void main() {
	int[5] data = {1, 2, 3, 4, 5};
	reverse_iter iter = {data.len-1, data};
	foreach(item, iter) {
		println(item);
	}
}
```

7. Variatics
```c++
// type... is same as type[]
string format<type... Args>(string fmt, Args args) {
	char[] buf;
	int i;
	foreach(ch, fmt) {
		if (ch != '{' && peek(fmt) != '}') {
			buf = append(buf, ch);
		} else {
			buf = append(buf, stringify(args[i++]));
		}
	}
	return {buf};
}

void main() {
	println(format("my name is {}", "ayush"));
}
```

## Built In types
```c++
alias string = const char[];
alias rune   = uint
alias size_t = ulong
```

## Built In procedures
```c++
T[] append<type T>(T[] arr, T value);
int copy<type T>(T[] dst, T[] src);
T* new<type T>();
T make<type T>();
void delete<type T>(T[]);
void delete<type T>(T*);
T min<type... T>(T);
T max<type... T>(T);
size sizeof<type T>(T);
size sizeof<type T>(T);
size offsetof<type T>(T);
size alignof<type T>(T);
string stringify<type T>(T);
void assert(int);
void assert(int, string message);
```