module CopyPropagation where

import Data.Map as Map

import BasicBlock
import IR
import OptimizationUtils

copyPropagation :: BBGraph -> BBGraph
copyPropagation g =
    let ir = findCopyAssignment (Map.elems (ids g))
        f (IR_Ass x v) = mapIR (modifyValue (\v' -> if v' == VarIR x then v else v'))
        f _ = id
     in (Prelude.foldl (.) id (Prelude.map f ir)) g

findCopyAssignment :: [BasicBlock] -> [IR]
findCopyAssignment bbs = concat (Prelude.map getXs bbs)
    where getXs (BB _ xs) = Prelude.filter isCopyAss xs

isCopyAss :: IR -> Bool
isCopyAss (IR_Ass _ _) = True
isCopyAss _ = False
