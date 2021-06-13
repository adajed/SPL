module FinishOptimizations where

import qualified Data.Map as M

import BasicBlock
import IR

import OptimizationUtils

finishOptimizations :: BBGraph -> BBGraph
finishOptimizations = (uncurry addSpecialTemps) . reorderTemps . removePhi

reorderTemps :: BBGraph -> (BBGraph, Int)
reorderTemps g = (mapIR f g, i)
    where f = (modifyVar h) . (modifyValue (remapValIR h))
          h x = case x of
                  SVar (VarA n) s -> SVar (VarA n) s
                  _ -> remap M.! x
          (remap, i) = foldl getMapBB (M.empty, 1) (M.elems (ids g))
          getMapBB m (BB _ xs) = foldl getMapIR m xs
          getMapIR m ir = foldl mapSVar m (getAllVars ir)
          mapSVar (m, i) x@(SVar _ s) = if M.member x m
                                            then (m, i)
                                            else (M.insert x (SVar (VarT i) s) m, i+1)

remapValIR :: (SVar -> SVar) -> ValIR -> ValIR
remapValIR f (VarIR x) = VarIR (f x)
remapValIR _ v = v

addSpecialTemps :: BBGraph -> Int -> BBGraph
addSpecialTemps g n = fst (foldl go (g, n) (M.keys (ids g)))
    where go (g', n') i = let (bb', n'') = goBB ((ids g') M.! i) n'
                            in (g' { ids = M.insert i bb' (ids g') }, n'')
          goBB (BB name xs) n' = let (xs', n'') = foldr f ([], n') xs
                                   in (BB name xs', n'')
          f ir (acc, n') = let (ir', n'') = fixIR (ir, n')
                             in (ir' ++ acc, n'')

fixIR :: (IR, Int) -> ([IR], Int)
fixIR (IR_BinOp IDiv x v1 i@(IntIR _ s), n') =
    let t = SVar (VarT n') s
      in ([IR_Ass t i, IR_BinOp IDiv x v1 (VarIR t)], n'+1)
fixIR (IR_BinOp IMod x v1 i@(IntIR _ s), n') =
    let t = SVar (VarT n') s
      in ([IR_Ass t i, IR_BinOp IMod x v1 (VarIR t)], n'+1)
fixIR (ir, n') = ([ir], n')

removePhi :: BBGraph -> BBGraph
removePhi g = foldl f g' phis
    where isPhi ir = case ir of { (IR_Phi _ _) -> True ; _ -> False }
          getPhis (BB _ xs) = filter isPhi xs
          phis = concat (map getPhis (M.elems (ids g)))
          h (BB name xs) = BB name (filter (not . isPhi) xs)
          g' = g { ids = M.map h (ids g) }
          f g (IR_Phi x vs) = insertPhiEquivalence x vs g
          f g _ = g

insertPhiEquivalence :: SVar -> [(Int, ValIR)] -> BBGraph -> BBGraph
insertPhiEquivalence x vs g = foldl f g vs
    where f g (i, v) = if v == VarIR x then g
                                      else insertAtTheEnd i (IR_Ass x v) g

insertAtTheEnd :: Int -> IR -> BBGraph -> BBGraph
insertAtTheEnd i ir g = g { ids = M.adjust f i (ids g) }
    where f (BB name []) = BB name [ir]
          f (BB name xs) = case last xs of
                             IR_Jump l -> BB name ((init xs) ++ [ir, IR_Jump l])
                             IR_CondJump v1 op v2 l1 l2 -> BB name ((init xs) ++ [ir, IR_CondJump v1 op v2 l1 l2])
                             _ -> BB name (xs ++ [ir])

