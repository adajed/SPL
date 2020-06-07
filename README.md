# Compiler for SPL written in Haskell

Compiler generates code for Linux x86_64.

## Compilation

### Prerequisites (with tested versions)
- bnfc 2.8 (https://bnfc.digitalgrammars.com/)
- happy 1.19.8
- alex 3.2.3
- make 4.1
- stack 2.1.3

- nasm 2.13.02
- gcc 7.5.0

### Buildling
`
make
`

## Features
- basic types (int, bool)
- array types
- structs
- functions
- lambda expression
- IR as quadruple code
- optimizations
  + copy propagation
  + dead code elimination
  + constant folding
  + local common subexpression elimination
  + arithmetic optimizations
  + global common subexpression elimination
  + unreachable code elimination
- assembler generation
  + register allocation based on graph coloring
  + generating assembler for nasm
  + linking with help of gcc

TODO:
- classes
- virtual functions
- algebraic types
- optmizations
  + strength reduction
  + layout optimization
- assembler code
  + assembler code optimizations
  
## Tests
To run tests just execute:
`
./scripts/test.sh
`
