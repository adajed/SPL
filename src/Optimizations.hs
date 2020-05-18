module Optimizations where

import BasicBlock

import ConstantFolding
import CopyPropagation
import DeadCodeElimination
import RemoveNop
import TrivialPhiElimination

optimize :: BBGraph -> BBGraph
optimize g = iterateUntilFixpoint opts g
    where opts = foldl (.) id fs
          fs = [ constantFolding
               , copyPropagation
               , trivialPhiElimination
               , deadCodeElimination
               , removeNop]


iterateUntilFixpoint :: (BBGraph -> BBGraph) -> BBGraph -> BBGraph
iterateUntilFixpoint f g =
    let newg = f g
     in if g == newg then g else iterateUntilFixpoint f newg
