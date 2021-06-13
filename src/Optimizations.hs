module Optimizations where

import Data.Map as Map


import ArithmeticOptimizations
import AbsSPL
import BasicBlock
import ConstantFolding
import CopyPropagation
import DeadCodeElimination
import GlobalCommonSubexpressionElimination
import LayoutOptimization ( layoutOptimization )
import LocalCommonSubexpressionElimination
import InlineFunctions ( inlineFunctions )
import RemoveNop
import RemoveVoidVariables
import Token
import TrivialPhiElimination
import UnreachableCodeElimination

data OptimizationOptions = OptimizationOptions { doConstantFolding :: Bool
                                               , doArithmeticOptimizations :: Bool
                                               , doCopyPropagation :: Bool
                                               , doDeadCodeElimination :: Bool
                                               , doCommonSubexpressionElimination :: Bool
                                               , doRegisterAllocation :: Bool
                                               , doInlineFunctions :: Bool
                                               }

basicOptimize :: OptimizationOptions -> Map VIdent BBGraph -> Map VIdent BBGraph
basicOptimize options = iterateUntilFixpoint (Map.map opts)
    where opts = removeNop
               . (if doDeadCodeElimination options then deadCodeElimination else id)
               . (if doCopyPropagation options then copyPropagation else id)
               . (if doConstantFolding options then constantFolding else id)
               . removeVoidVariables

optimize :: OptimizationOptions -> Map VIdent BBGraph -> Map VIdent BBGraph
optimize options = iterateUntilFixpoint opts
    where opts = (Map.map layoutOptimization)
               . (if doInlineFunctions options then inlineFunctions else id)
               . (Map.mapWithKey unreachableCodeElimination)
               . (Map.map (optimizeBBGraph options))

optimizeBBGraph :: OptimizationOptions -> BBGraph -> BBGraph
optimizeBBGraph options = iterateUntilFixpoint opts
    where opts = removeNop
               . (if doCommonSubexpressionElimination options then globalCommonSubexpressionElimination else id)
               . (if doCommonSubexpressionElimination options then localCommonSubexpressionElimination else id)
               . (if doArithmeticOptimizations options then arithmeticOptimizations else id)
               . (if doDeadCodeElimination options then deadCodeElimination else id)
               . trivialPhiElimination
               . (if doCopyPropagation options then copyPropagation else id)
               . (if doConstantFolding options then constantFolding else id)

iterateUntilFixpoint :: Eq a => (a -> a) -> a -> a
iterateUntilFixpoint f x =
    let x' = f x
     in if x == x' then x
                   else iterateUntilFixpoint f x'
