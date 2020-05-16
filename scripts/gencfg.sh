#!/bin/bash

bnfc --haskell -o src --functor grammar/SPL.cf
happy -o src/ParSPL.hs --info=grammar/happy.info -gca src/ParSPL.y
alex -o src/LexSPL.hs -g src/LexSPL.x
rm src/LexSPL.x src/ParSPL.y src/TestSPL.hs
