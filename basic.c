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

#define unreachable assert(0);
#define todo(message) assert(0 && "TODO:" message);

typedef struct {
  int len;
  char* ptr;
} string;

typedef struct {
  int len;
  int cap;
  char* ptr;
} string_builder;

typedef struct {
  int len;
  int cap;
  string* ptr;
} strings;

#define sv(cstr) (string){sizeof(cstr)-1, cstr}
#define string_fmt "%.*s"
#define string_arg(s) (int)(s).len, (s).ptr
#define string_empty (string){0};

bool string_eq(string s1, string s2) {
  return s1.len == s2.len && memcmp(s1.ptr, s2.ptr, s1.len) == 0;
}

uint32_t string_hash(string key) {
  uint32_t h = 0;
  for (int i=0; i<key.len; i++) {
    h = h * 37 + (uint8_t)key.ptr[i];
  }
  return h;
}

typedef struct {
  string first;
  string second;
} string_pair;

string_pair string_split_first(string s, char delim) {
  int index = -1;
  for (int i=0; i<s.len; i++) {
    if (s.ptr[i] == delim) {
      index=i; 
      break;
    }
  }

  string_pair pair = {0};
  if (index == -1) {
    pair.first = s;
    pair.second = string_empty;
  } else {
    pair.first = (string){index, s.ptr};
    pair.second = (string){s.len-index, s.ptr+index};
  }
  return pair;
}

string_pair string_split_last(string s, char delim) {
  int index = -1;
  for (int i=s.len-1; i>=0; i--) {
    if (s.ptr[i] == delim) {
      index=i; 
      break;
    }
  }

  string_pair pair = {0};
  if (index == -1) {
    pair.first = s;
    pair.second = string_empty;
  } else {
    pair.first = (string){index, s.ptr};
    pair.second = (string){s.len-index, s.ptr+index};
  }
  return pair;
}

void print(string s) {
  printf("%.*s", string_arg(s));
}

void println(string s) {
  printf("%.*s\n", string_arg(s));
}

#define TEMP_BUFFER_CAP (1024*1024)

_Thread_local size_t temp_allocated = 0;
_Thread_local char temp_buffer[TEMP_BUFFER_CAP];

size_t align(size_t size, size_t alignment) {
  assert(alignment != 0);
  if (size % alignment == 0)
    return size;
  return size + (alignment - size % alignment);
}

void *talloc(size_t n) {
  size_t size = align(n, 8);
  assert(size <= TEMP_BUFFER_CAP);

  if (temp_allocated + size >= TEMP_BUFFER_CAP) {
    temp_allocated = 0;
  }

  void *ptr = &temp_buffer[temp_allocated];
  temp_allocated += size;
  return ptr;
}

void treset() { temp_allocated = 0; }

__attribute__ ((format (printf, 1, 2)))
string tsprintf(const char *format, ...) {
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

const char* string_to_cstr(string s) {
  char* cstr = talloc(s.len);
  memcpy(cstr, s.ptr, s.len);
  return cstr;
}

int64_t string_to_number(string s) {
  const char* cstr = string_to_cstr(s);
  return (int64_t)strtoll(cstr, (char **)NULL, 10);
}

string strconcat(string a, string b) {
  char* ptr = malloc(a.len+b.len+1);
  memcpy(ptr, a.ptr, a.len);
  memcpy(ptr+a.len, b.ptr, b.len);
  ptr[a.len+b.len] = 0;
  return (string){a.len+b.len, ptr};
}

char character_escape(string s) {
  if (s.len == 1) return s.ptr[0];
  else {
    if (string_eq(s, sv("\\\\"))) {
      return '\\';
    } else if (string_eq(s, sv("\\n"))) {
      return '\n';
    } else if (string_eq(s, sv("\\r"))) {
      return '\r';
    } else if (string_eq(s, sv("\\t"))) {
      return '\t';
    } else if (string_eq(s, sv("\\b"))) {
      return '\b';
    } else if (string_eq(s, sv("\\f"))) {
      return '\f';
    } else if (string_eq(s, sv("\\'"))) {
      return '\'';
    } else if (string_eq(s, sv("\\\""))) {
      return '\"';
    }
  }
  return 0;
}

string character_unescape(char ch) {
  char* cstr = talloc(2);
  int len = 0;
  switch(ch) {
  case '\\': {
    cstr[len++] = '\\';
    cstr[len++] = '\\';
  } break;
  
  case '\"': {
    cstr[len++] = '\\';
    cstr[len++] = '\"';
  } break;
  
  case '\'': {
    cstr[len++] = '\\';
    cstr[len++] = '\'';
  } break;
  
  case '\n': {
    cstr[len++] = '\\';
    cstr[len++] = 'n';
  } break;

  case '\r': {
    cstr[len++] = '\\';
    cstr[len++] = 'r';
  } break;
  
  case '\t': {
    cstr[len++] = '\\';
    cstr[len++] = 't';
  } break;
  
  case '\b': {
    cstr[len++] = '\\';
    cstr[len++] = 'b';
  } break;
  
  case '\f': {
    cstr[len++] = '\\';
    cstr[len++] = 'f';
  } break;
  
  default: {
      cstr[len++] = ch;
  } break;
  }
  if (ch == '\\' || ch == '\"' || ch == '\'' 
    || ch == '\n' || ch == '\r' || ch == '\t' 
    || ch == '\b' || ch == '\f') {
  }
  return (string){len, cstr};
}

string string_unescape(string s) {
  char* cstr = talloc(s.len*2+1);
  int len = 0;

  for (int i=0; i<s.len; i++) {
    string ch = character_unescape(s.ptr[i]);
    for (int j=0; j<ch.len; j++) {
      cstr[len++] = ch.ptr[j];
    }
  }
  cstr[len++] = 0; 
  return (string){len, cstr};
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
  if (stat(path, &st) == 0) return st.st_size;
  return 0;
}

bool file_exists(const char *path) {
  struct stat st;
  return stat(path, &st) == 0;
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
            int new_cap = (array)->cap == 0 ? 3 : (int)((array)->cap * 1.5); \
            typeof((array)->ptr) new_ptr = realloc((array)->ptr, new_cap * sizeof(*(array)->ptr)); \
            assert(new_ptr != NULL && "failed to allocate memory"); \
            (array)->ptr = new_ptr; \
            (array)->cap = new_cap; \
        } \
        (array)->ptr[(array)->len++] = (item); \
    } while (0)

typedef struct {
  bool occupied;
  string key;
  uint8_t item[];
} strmap_entry;

typedef struct {
  int len;
  int cap;
  int item_size;
  strmap_entry* ptr;
} strmap;

#define strmap_stride(item_size) (sizeof(strmap_entry) + (item_size))
#define strmap_entry(map, index) ((strmap_entry*)((map)->ptr + (index) * strmap_stride((map)->item_size)))
#define strmap_value(entry) ((entry)->item)

#define strmap_new(T, cap) strmap_new_(sizeof(T), cap)
strmap strmap_new_(int item_size, int cap) {
  size_t stride = strmap_stride(item_size);
  strmap_entry* ptr = cap > 0 ? calloc(cap, stride) : NULL;
  return (strmap){0, cap, item_size, ptr};
}

typedef struct {
  int index;
  strmap map;
} strmap_iter;

void* strmap_get_next(strmap_iter* iter) {
  assert(iter->map.ptr != NULL);
  assert(iter->map.item_size != 0);

  while (iter->index < iter->map.cap) {
    strmap_entry* entry = strmap_entry(&iter->map, iter->index);
    if (entry->occupied) {
      iter->index++;
      return strmap_value(entry);
    }
    iter->index++;
  }
  return NULL;
}

void* strmap_get(strmap* map, string key) {
  assert(map != NULL);
  assert(map->item_size != 0);
  if (map->cap == 0) return NULL;

  uint32_t hash = string_hash(key);
  for (int i=0; i<map->cap; i++) {
    size_t slot = (i + hash) % map->cap;
    strmap_entry* entry = strmap_entry(map, slot);
    if (!entry->occupied) return NULL;
    
    if (string_eq(entry->key, key)) {
      return strmap_value(entry);
    }
  }
  return NULL;
}

// keys must not go out of scope
void strmap_put(strmap* map, string key, void* item) {
  assert(map != NULL);
  assert(map->item_size != 0);

  if (map->cap == 0 || map->len >= map->cap * 0.7) {
    // reashing and resizing
    strmap_iter iter = {0, *map};
    map->len = 0;
    map->cap = map->cap == 0 ? 4 : (int)(map->cap * 1.5);
    size_t stride = strmap_stride(map->item_size);
    map->ptr = malloc(map->cap*stride);
    memset(map->ptr, 0, map->cap*stride);

    void* entry = strmap_get_next(&iter);
    while(entry != NULL) {
      strmap_put(map, key, entry);
      entry = strmap_get_next(&iter);
    }
    if (iter.map.ptr != NULL) free(iter.map.ptr);
  }
  
  uint32_t hash = string_hash(key);
  for (int i=0; i<map->cap; i++) {
    size_t slot = (i + hash) % map->cap;
    strmap_entry* entry = strmap_entry(map, slot);
    
    if (!entry->occupied || string_eq(entry->key, key)) {
      if (!entry->occupied) map->len++;
      entry->occupied = true;
      entry->key = key;
      memcpy(strmap_value(entry), item, map->item_size);
      return;
    }
  }
  assert(0 && "table full");
}

bool strmap_remove(strmap* map, string key) {
  assert(map != NULL);
  assert(map->item_size != 0);
  if (map->cap == 0) return NULL;

  uint32_t hash = string_hash(key);
  for (int i=0; i<map->cap; i++) {
    size_t slot = (i + hash) % map->cap;
    strmap_entry* entry = strmap_entry(map, slot);
    if (!entry->occupied) return NULL;
    
    if (string_eq(entry->key, key)) {
      entry->occupied = false;
      return true;
    }
  }

  return false;
}

void strmap_free(strmap* map) {
  if (map->ptr != NULL) {
    free(map->ptr);
    map->ptr = NULL;
    map->len = 0;
    map->cap = 0;
  }
}

void report_error_old(string message) {
  error_count++;
  print(sv("error: "));
  println(message);
}

void cmd(string command) {
  fprintf(stderr, "CMD: %.*s\n", string_arg(command));
  system(string_to_cstr(command));
}