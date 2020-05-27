all: lib/runtime.o spl

.PHONY:
	spl runtime clean

lib/runtime.o: lib/runtime.c
	gcc -c -o lib/runtime.o lib/runtime.c

spl:
	stack build
	ln -s ./.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/spl/spl spl

clean:
	rm -f spl lib/runtime.o
	stack clean --full

