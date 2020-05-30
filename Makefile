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
	rm -f src/*.bak
	@sed -i 's/newtype CIdent = CIdent String deriving (Eq, Ord, Show, Read)/newtype CIdent = CIdent String deriving (Eq, Ord, Read)/' src/AbsSPL.hs
	@sed -i 's/newtype VIdent = VIdent String deriving (Eq, Ord, Show, Read)/newtype VIdent = VIdent String deriving (Eq, Ord, Read)/' src/AbsSPL.hs
	@sed -i '11iinstance Show CIdent where' src/AbsSPL.hs
	@sed -i '12ishow (CIdent name) = name' src/AbsSPL.hs
	@sed -i '12s/^/    /' src/AbsSPL.hs
	@sed -i '14iinstance Show VIdent where' src/AbsSPL.hs
	@sed -i '15ishow (VIdent name) = name' src/AbsSPL.hs
	@sed -i '15s/^/    /' src/AbsSPL.hs
	@sed -i '12iimport Data.Char (ord)' src/LexSPL.hs

clean:
	rm -f spl lib/runtime.o
	stack clean --full

