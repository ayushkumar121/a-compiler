#include <stdio.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdarg.h>

#define max(a,b) (a>b?a:b)
#define min(a,b) (a<b?a:b)

#define TEMP_BUFFER_CAP (1024*1024)

_Thread_local size_t temp_allocated = 0;
_Thread_local char temp_buffer[TEMP_BUFFER_CAP];

size_t align(size_t size) {
  if (size % 8 == 0)
    return size;
  return size + (8 - size % 8);
}

void *talloc(size_t n) {
  size_t size = align(n);
  assert(size <= TEMP_BUFFER_CAP);

  if (temp_allocated + size >= TEMP_BUFFER_CAP) {
    temp_allocated = 0;
  }

  void *ptr = &temp_buffer[temp_allocated];
  temp_allocated += size;
  return ptr;
}

void treset() { temp_allocated = 0; }

typedef struct {
	size_t len;
	char* ptr;
} string;

#define sv(cstr) (string){sizeof(cstr)-1, cstr}
#define string_fmt "%.*s"
#define string_arg(s) (int)(s).len, (s).ptr
#define string_empty (string){0};

__attribute__ ((format (printf, 1, 2)))
string tprintf(const char *format, ...) {
  va_list args, args2;

  va_start(args, format);
  va_copy(args2, args);

  size_t n = vsnprintf(NULL, 0, format, args);
  assert(n > 0);

  char *ptr = talloc(n + 1);
  vsnprintf(ptr, n + 1, format, args2);
  ptr[n] = 0;

  va_end(args2);
  va_end(args);

  return (string){n, ptr};
}

string tconcat(string a, string b) {
  char* ptr = talloc(a.len+b.len+1);
  memcpy(ptr, a.ptr, a.len);
  memcpy(ptr+a.len, b.ptr, b.len);
  ptr[a.len+b.len] = 0;
  return (string){a.len+b.len, ptr};
}

typedef struct {
	int index;
	int argc;
	char** argv;
} Args;

string args_next(Args* args) {
	if (args->index < args->argc) {
		char* cstr = args->argv[args->index++];
		size_t n = strlen(cstr);
		return (string){n, cstr};
	}
	return string_empty;
}

size_t file_size(const char *path) {
  struct stat st;
  if (stat(path, &st) == 0) {
    return st.st_size;
  }
  return 0;
}

bool file_exists(const char *path) {
  struct stat st;
  if (stat(path, &st) == 0) {
    return true;
  }
  return false;
}

// Allocates memory on the heap
string file_read_to_string(const char* path) {
	size_t size = file_size(path);
  if (size == 0) {
    return string_empty;
  }

  char* buffer = malloc(size);
  FILE *file = fopen(path, "r");
  if (file == NULL) {
  	fprintf(stderr, "failed to open file %s: %s", path, strerror(errno));
    return string_empty;
  }

  size_t n = fread(buffer, 1, size, file);
  if (n != size) {
  	fprintf(stderr, "failed to read file %s: %s", path, strerror(errno));
    return string_empty;
  }
  fclose(file);

  return (string){size, buffer};
}

#define array_append(array, item) \
    do { \
        if ((array)->len >= (array)->cap) { \
            int new_cap = (array)->cap == 0 ? 2 : (array)->cap * 1.5; \
            typeof((array)->ptr) new_ptr = realloc((array)->ptr, new_cap * sizeof(*(array)->ptr)); \
            assert(new_ptr != NULL && "failed to allocate memory"); \
            memcpy(new_ptr, (array)->ptr, (array)->len * sizeof(*(array)->ptr)); \
            (array)->ptr = new_ptr; \
            (array)->cap = new_cap; \
        } \
        (array)->ptr[(array)->len++] = (item); \
    } while(0)



