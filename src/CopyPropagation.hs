module CopyPropagation where

import qualified Data.Map as M

import BasicBlock
import IR
import OptimizationUtils

copyPropagation :: BBGraph -> BBGraph
copyPropagation g =
    let ir = findCopyAssignment (M.elems (ids g))
        f g' (IR_Ass x v) =
            mapIR (modifyValue (\v' -> if v' == VarIR x then v else v')) g'
        f g' _ = g'
     in foldl f g ir

findCopyAssignment :: [BasicBlock] -> [IR]
findCopyAssignment bbs = concat (map getXs bbs)
    where getXs (BB _ xs) = filter isCopyAss xs

isCopyAss :: IR -> Bool
isCopyAss (IR_Ass _ _) = True
isCopyAss _ = False
