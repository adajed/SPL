
SRC=$(wildcard src/*.hs) $(wildcard app/*.hs)

all: lib/runtime.o spl

.PHONY: clean

lib/runtime.o: lib/runtime.asm
	nasm -felf64 -o lib/runtime.o lib/runtime.asm

spl: $(SRC)
	stack build #--trace --no-strip --profile
	rm -f spl
	ln -s ./.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/spl/spl spl

src/LexSPL.hs: grammar/LexSPL.x
	alex -o src/LexSPL.hs -g grammar/LexSPL.x

src/ParSPL.hs: grammar/ParSPL.y
	happy -o src/ParSPL.hs --info=grammar/happy.info -gca grammar/ParSPL.y

clean:
	rm -f spl lib/runtime.o
	stack clean --full

