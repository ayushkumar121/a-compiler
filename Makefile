CFLAGS=-Wall -Wextra -Werror -g -fsanitize=address -fsanitize=undefined
.PHONY: clean

all: tinyc tests

tinyc: tinyc.c basic.c lexer.c parser.c
	cc -o tinyc tinyc.c $(CFLAGS)

tests: tests.c basic.c lexer.c parser.c
	cc -o tests tests.c $(CFLAGS)

clean:
	rm tinyc