# Compiler for SPL written in Haskell


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
  + arithmentic optimizations
  + global common subexpression elimination
- generate assembler
  + register allocation based on graph coloring

TODO:
- classes
- virtual functions
- algebraic types
- optmizations
  + strength reduction
- assembler code
  + assembler code optimizations
