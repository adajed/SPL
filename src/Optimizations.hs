module Optimizations where

import BasicBlock

import ArithmeticOptimizations
import ConstantFolding
import CopyPropagation
import DeadCodeElimination
import LocalCommonSubexpressionElimination
import RemoveNop
import TrivialPhiElimination

optimize :: BBGraph -> BBGraph
optimize g = head (iterateUntilFixpoint opts g)
    where opts = foldl (.) id fs
          fs = [ localCommonSubexpressionElimination
               , constantFolding
               , arithmeticOptimizations
               , copyPropagation
               , trivialPhiElimination
               , deadCodeElimination
               , removeNop]


iterateUntilFixpoint :: (BBGraph -> BBGraph) -> BBGraph -> [BBGraph]
iterateUntilFixpoint f g = help [g] g
    where help xs x =
            let newx = f x
              in if x == newx then xs else help (newx:xs) newx
