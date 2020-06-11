module TrivialPhiElimination where

import qualified Data.Map as M
import qualified Data.Set as S

import BasicBlock
import IR
import OptimizationUtils

trivialPhiElimination :: BBGraph -> BBGraph
trivialPhiElimination =
        trivialPhiElimination_allTheSame .
            trivialPhiElimination_loop .
            trivialPhiElimination_zero

trivialPhiElimination_zero :: BBGraph -> BBGraph
trivialPhiElimination_zero = mapIR f
    where f (IR_Phi _ []) = IR_Nop
          f ir = ir

trivialPhiElimination_loop :: BBGraph -> BBGraph
trivialPhiElimination_loop = mapIR f
    where f (IR_Phi x vs) = IR_Phi x (filter ((/= (VarIR x)) . snd) vs)
          f ir = ir

trivialPhiElimination_allTheSame :: BBGraph -> BBGraph
trivialPhiElimination_allTheSame = mapIR f
    where f ir@(IR_Phi x ((_, v):vs)) =
            if all ((==v) . snd) vs then IR_Ass x v else ir
          f ir = ir
