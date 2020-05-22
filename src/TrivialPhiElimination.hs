module TrivialPhiElimination where

import Data.Map as Map

import BasicBlock
import IR
import OptimizationUtils

trivialPhiElimination :: BBGraph -> BBGraph
trivialPhiElimination =
        trivialPhiElimination_allTheSame .
            trivialPhiElimination_loop .
            trivialPhiElimination_zero

trivialPhiElimination_zero :: BBGraph -> BBGraph
trivialPhiElimination_zero g =
    let phi (IR_Phi _ []) = True
        phi ir = False
        g' = mapIR (\ir -> if phi ir then IR_Nop else ir) g
        ir = concat (Prelude.map (\(BB _ xs) -> Prelude.filter phi xs) (Map.elems (ids g)))
     in Prelude.foldl remove g' ir

remove :: BBGraph -> IR -> BBGraph
remove g (IR_Phi x []) = mapIR f g
    where f (IR_Phi x' vs) = IR_Phi x' (Prelude.filter (not . (==(VarIR x)) . snd) vs)
          f ir = ir
remove g _ = g

trivialPhiElimination_loop :: BBGraph -> BBGraph
trivialPhiElimination_loop = mapIR f
    where f (IR_Phi x vs) = IR_Phi x (Prelude.filter ((/= (VarIR x)) . snd) vs)
          f ir = ir

trivialPhiElimination_allTheSame :: BBGraph -> BBGraph
trivialPhiElimination_allTheSame = mapIR f
    where f ir@(IR_Phi x ((_, v):vs)) =
            if all ((==v) . snd) vs then IR_Ass x v else ir
          f ir = ir

