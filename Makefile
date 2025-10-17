CFLAGS=-Wall -Wextra -g -Wswitch
.PHONY: clean

all: tinyc

tinyc: tinyc.c basic.c lexer.c parser.c
	cc -o tinyc tinyc.c $(CFLAGS)

clean:
	rm tinyc