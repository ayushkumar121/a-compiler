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

typedef struct {
  int len;
  char* ptr;
} string;

#define sv(cstr) (string){sizeof(cstr)-1, cstr}
#define string_fmt "%.*s"
#define string_arg(s) (int)(s).len, (s).ptr
#define string_empty (string){0};

bool string_eq(string s1, string s2) {
  return s1.len == s2.len && memcmp(s1.ptr, s2.ptr, s1.len);
}

uint32_t string_hash(string key) {
  uint32_t h = 0;
  for (int i=0; i<key.len; i++) {
    h = h * 37 + (uint8_t)key.ptr[i];
  }
  return h;
}

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
            int new_cap = (array)->cap == 0 ? 3 : (int)((array)->cap * 1.5); \
            typeof((array)->ptr) new_ptr = realloc((array)->ptr, new_cap * sizeof(*(array)->ptr)); \
            assert(new_ptr != NULL && "failed to allocate memory"); \
            memcpy(new_ptr, (array)->ptr, (array)->len * sizeof(*(array)->ptr)); \
            (array)->ptr = new_ptr; \
            (array)->cap = new_cap; \
        } \
        (array)->ptr[(array)->len++] = (item); \
    } while(0)

typedef struct {
  bool occupied;
  string key;
} string_map_entry;

typedef struct {
  int len;
  int cap;
  int stride;
  string_map_entry* ptr;
} string_map;

#define smap_entry(map, i) (string_map_entry*)((uint8_t*)(map)->ptr + (i) * (map)->stride)
#define smap_value(map, entry_ptr) ((uint8_t*)(entry_ptr) + sizeof(string_map_entry))

#define smap_new(T, cap) map_new_(sizeof(T), cap)
string_map map_new_(int item_size, int cap) {
  int stride = 1+sizeof(string)+item_size;
  return (string_map){0, cap, stride, malloc(stride*cap)};
}

typedef struct {
  int index;
  string_map map;
} string_map_iter;

void* smap_get_next(string_map_iter* iter) {
  assert(iter->map.ptr != NULL);
  assert(iter->map.stride != 0);

  while (iter->index < iter->map.cap) {
    string_map_entry* entry = smap_entry(&iter->map, iter->index);
    if (entry->occupied) {
      iter->index++;
      return smap_value(iter->map, entry);
    }
    iter->index++;
  }
  return NULL;
}

void* smap_get(string_map* map, string key) {
  assert(map != NULL);
  assert(map->stride != 0);
  if (map->cap == 0) return NULL;

  uint32_t hash = string_hash(key);
  for (int i=0; i<map->cap; i++) {
    size_t slot = (i + hash) % map->cap;
    string_map_entry* entry = smap_entry(map, slot);
    if (!entry->occupied) return NULL;
    
    if (string_eq(entry->key, key)) {
      return smap_value(map, entry);
    }
  }
  return NULL;
}

// keys must not go out of scope
void smap_put(string_map* map, string key, void* item) {
  assert(map != NULL);
  assert(map->stride != 0);

  if (map->cap == 0 || map->len >= map->cap * 0.7) {
    // reashing and resizing
    string_map_iter iter = {0, *map};
    map->len = 0;
    map->cap = map->cap == 0 ? 4 : (int)(map->cap * 1.5);
    map->ptr = malloc(map->cap*map->stride);
    memset(map->ptr, 0, map->cap*map->stride);

    void* entry = smap_get_next(&iter);
    while(entry != NULL) {
      smap_put(map, key, entry);
      entry = smap_get_next(&iter);
    }
    if (iter.map.ptr != NULL) free(iter.map.ptr);
  }
  
  uint32_t hash = string_hash(key);
  for (int i=0; i<map->cap; i++) {
    size_t slot = (i + hash) % map->cap;
    string_map_entry* entry = smap_entry(map, slot);
    
    if (!entry->occupied || string_eq(entry->key, key)) {
      if (!entry->occupied) map->len++;
      entry->occupied = true;
      entry->key = key;
      memcpy(smap_value(map, entry), item, map->stride - sizeof(*entry));
      return;
    }
  }
  assert(0 && "table full");
}

void smap_free(string_map* map) {
  if (map->ptr != NULL) {
    free(map->ptr);
    map->ptr = NULL;
    map->len = 0;
    map->cap = 0;
  }
}
