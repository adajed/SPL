all: lib/runtime.o spl

.PHONY: spl clean

lib/runtime.o: lib/runtime.c
	gcc -c -o lib/runtime.o lib/runtime.c

spl: grammar
	stack build
	rm -f spl
	ln -s ./.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/spl/spl spl

grammar: src/AbsSPL.hs

src/AbsSPL.hs : grammar/SPL.cf
	./scripts/gencfg.sh
	@sed -i 's/newtype Ident = Ident String deriving (Eq, Ord, Show, Read)/newtype Ident = Ident String deriving (Eq, Ord, Read)/' src/AbsSPL.hs
	@sed -i '11iinstance Show Ident where' src/AbsSPL.hs
	@sed -i '12ishow (Ident name) = name' src/AbsSPL.hs
	@sed -i '12s/^/    /' src/AbsSPL.hs

clean:
	rm -f spl lib/runtime.o
	stack clean --full

