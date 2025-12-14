typedef struct {
	int cap;
	int len;
	uint8_t* ptr;
} arena;

void arena_init(arena* a, int cap) {
	a->len = 0;
	a->cap = cap;
	a->ptr = malloc(a->cap);
}

void* arena_alloc(arena* a, int sz) {
	int aligned = align(sz, 8);
	ASSERT(a->len + aligned <= a->cap);
	void* ptr = a->ptr + a->len;
	a->len += aligned;
	return ptr;
}

void arena_free(arena* a) {
	if (a->ptr) {
		free(a->ptr);
		a->len = 0;
		a->cap = 0;
		a->ptr = NULL;
	}
}

#define GLOBAL_ARENA_CAP (1000*1000*10) // 10MB
arena ga = {0};