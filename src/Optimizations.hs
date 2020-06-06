module Optimizations where

import Data.Map as Map


import ArithmeticOptimizations
import AbsSPL
import BasicBlock
import ConstantFolding
import CopyPropagation
import DeadCodeElimination
import GlobalCommonSubexpressionElimination
import LocalCommonSubexpressionElimination
import RemoveNop
import TrivialPhiElimination
import UnreachableCodeElimination

optimize :: Map VIdent BBGraph -> Map VIdent BBGraph
optimize program = iterateUntilFixpoint opts program
    where opts = (Map.mapWithKey unreachableCodeElimination)
               . (Map.map optimizeBBGraph)

optimizeBBGraph :: BBGraph -> BBGraph
optimizeBBGraph g = iterateUntilFixpoint opts g
    where opts = Prelude.foldl (.) id fs
          fs = [ globalCommonSubexpressionElimination
               , localCommonSubexpressionElimination
               , constantFolding
               , arithmeticOptimizations
               , copyPropagation
               , trivialPhiElimination
               , deadCodeElimination
               , removeNop ]


iterateUntilFixpoint :: Eq a => (a -> a) -> a -> a
iterateUntilFixpoint f x =
    let x' = f x
     in if x == x' then x
                   else iterateUntilFixpoint f x'
