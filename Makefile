CFLAGS=-Wall -Werror -g -fsanitize=address -fsanitize=undefined
.PHONY: clean

all: tinyc tests

tinyc: tinyc.c basic.c lexer.c parser.c compiler.c codegen.c simulator.c codegen/*.c
	cc -o tinyc tinyc.c $(CFLAGS)

tests: tests.c basic.c lexer.c parser.c compiler.c codegen.c simulator.c codegen/*.c
	cc -o tests tests.c $(CFLAGS)

foo: foo.asm
	as -o foo.o foo.asm
	ld -o foo foo.o -lSystem -syslibroot $(shell xcrun --show-sdk-path) -e _start

clean:
	rm tinyc
