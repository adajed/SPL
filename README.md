# Compiler for SPL written in Haskell


## Features
- basic types (int, bool)
- array types
- structs
- functions
- IR as quadruple code
- optimizations
  + copy propagation
  + dead code elimination
  + constant folding
  + local common subexpression elimination
  + arithmentic optimizations
- generate basic assembler code

TODO:
- classes
- virtual functions
- lambda expression
- algebraic types
- optmizations
  + strength reduction
  + global common subexpression elimination
- assembler code
  + register allocation
  + assembler code optimizations
