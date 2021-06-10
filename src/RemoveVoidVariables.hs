module RemoveVoidVariables (
    removeVoidVariables
    ) where

import qualified Data.Map as M

import BasicBlock
import IR

removeVoidVariables :: BBGraph -> BBGraph
removeVoidVariables bbgraph = bbgraph { ids = M.map fixVoidCalls (ids bbgraph) }

fixVoidCalls :: BasicBlock -> BasicBlock
fixVoidCalls (BB name xs) = BB name (map fix xs)
    where fix ir@(IR_Call y f xs) = case y of
                                   SVar _ 0 -> IR_VoidCall f xs
                                   _ -> ir
          fix ir = ir
