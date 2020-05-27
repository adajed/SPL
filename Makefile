all: lib/runtime.o compiler

.PHONY:
	compiler runtime clean

lib/runtime.o: lib/runtime.c
	gcc -c -o lib/runtime.o lib/runtime.c

compiler:
	stack build
	rm -f spl
	ln -s ./.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/spl/spl spl

grammar:
	./scripts/gencfg.sh

clean:
	rm -f spl lib/runtime.o
	stack clean --full

