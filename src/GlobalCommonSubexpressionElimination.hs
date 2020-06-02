module GlobalCommonSubexpressionElimination (
    globalCommonSubexpressionElimination
                                            ) where

import qualified Data.Map as Map
import qualified Data.List as List

import BasicBlock
import IR
import OptimizationUtils

globalCommonSubexpressionElimination :: BBGraph -> BBGraph
globalCommonSubexpressionElimination g = foldr f g paths
    where paths = allPaths g
          f p = (elimMemory p) . (elimTriples p) . (elimPairs p)

-- eliminate pairs -----------------------------------------

elimPairs :: [Int] -> BBGraph -> BBGraph
elimPairs xs g = foldl (doElimPair (last xs)) g c
    where ir1 = getIR g (head xs)
          ir2 = getIR g (last xs)
          c = filter (canElimPair . h) (pairs ir1 ir2)
          h ((_, a), (_, b)) = (a, b)

canElimPair :: (IR, IR) -> Bool
canElimPair (IR_BinOp op _ v1 v2, IR_BinOp op' _ v1' v2') =
    op == op' && v1 == v1' && v2 == v2'
canElimPair (IR_UnOp op _ v, IR_UnOp op' _ v') =
    op == op' && v == v'
canElimPair _ = False

doElimPair :: Int -> BBGraph -> (Ind IR, Ind IR) -> BBGraph
doElimPair i g ((_, IR_BinOp _ x1 _ _)
               ,(p, IR_BinOp _ x2 _ _)) =
    changeBBGraph i p (IR_Ass x2 (VarIR x1)) g
doElimPair i g ((_, IR_UnOp _ x1 _)
               ,(p, IR_UnOp _ x2 _)) =
    changeBBGraph i p (IR_Ass x2 (VarIR x1)) g
doElimPair _ g _ = g

-- eliminate triples ----------------------------------------------

elimTriples :: [Int] -> BBGraph -> BBGraph
elimTriples xs g = foldl (doElimTriple (last xs)) g c
    where ir1 = getIR g (head xs)
          ir2 = getIR g (last xs)
          c = filter (canElimTriple . h) (triples ir1 ir2)
          h (a, b, (_, c)) = (a, b, c)


canElimTriple :: (IR, IR, IR) -> Bool
canElimTriple (IR_BinOp (BOpInt ILshift) x1 v1 (IntIR n1 _)
              ,IR_BinOp (BOpInt IAdd)    x2 v2 (IntIR n2 _)
              ,IR_BinOp (BOpInt ILshift) x3 v3 (IntIR n3 _)) =
    v1 == v2 && (VarIR x2) == v3 && n1 == n3
canElimTriple (IR_BinOp (BOpInt IMul) x1 v1 (IntIR n1 _)
              ,IR_BinOp (BOpInt IAdd) x2 v2 (IntIR n2 _)
              ,IR_BinOp (BOpInt IMul) x3 v3 (IntIR n3 _)) =
    v1 == v2 && (VarIR x2) == v3 && n1 == n3
canElimTriple (IR_BinOp (BOpInt IAdd) x1 v1 w1
              ,IR_BinOp (BOpInt IAdd) x2 v2 w2
              ,IR_BinOp (BOpInt IAdd) x3 v3 w3) =
    v1 == v3 && w1 == v2 && (VarIR x2) == w3
canElimTriple _ = False

doElimTriple :: Int -> BBGraph -> (IR, IR, Ind IR) -> BBGraph
doElimTriple i g (    IR_BinOp (BOpInt ILshift) x1 v1 (IntIR n1 s)
                 ,    IR_BinOp (BOpInt IAdd)    x2 v2 (IntIR n2 _)
                 ,(p, IR_BinOp (BOpInt ILshift) x3 v3 (IntIR n3 _))) =
    changeBBGraph i p (IR_BinOp (BOpInt IAdd) x3 (VarIR x1) n) g
        where n = IntIR ((2 ^ n1) * n2) s
doElimTriple i g (    IR_BinOp (BOpInt IMul) x1 v1 (IntIR n1 s)
                 ,    IR_BinOp (BOpInt IAdd) x2 v2 (IntIR n2 _)
                 ,(p, IR_BinOp (BOpInt IMul) x3 v3 (IntIR n3 _))) =
    changeBBGraph i p (IR_BinOp (BOpInt IAdd) x3 (VarIR x1) n) g
        where n = IntIR (n1 * n2) s
doElimTriple i g (    IR_BinOp (BOpInt IAdd) x1 v1 w1
                 ,    IR_BinOp (BOpInt IAdd) x2 v2 w2
                 ,(p, IR_BinOp (BOpInt IAdd) x3 v3 w3)) =
    changeBBGraph i p (IR_BinOp (BOpInt IAdd) x3 (VarIR x1) w2) g
doElimTriple _ g _ = g

-- eliminate memory access -----------------------------------------

elimMemory :: [Int] -> BBGraph -> BBGraph
elimMemory path g = foldl (doElimMemory (last path)) g c
    where ir1 = getIR g (head path)
          ir2 = getIR g (last path)
          ir = concat (map (getIR g) (middle path))
          c = filter (canElimMemory ir1 ir ir2) (pairs ir1 ir2)

canElimMemory :: [IR] -> [IR] -> [IR] -> (Ind IR, Ind IR) -> Bool
canElimMemory irH irM irT ((n1, IR_MemRead _ v)
                          ,(n2, IR_MemRead _ v')) =
    v == v' && noMemSave (drop (n1 + 1) irH)
            && noMemSave (take n2 irT)
            && noMemSave irM
canElimMemory irH irM irT ((n1, IR_MemSave v _ _)
                          ,(n2, IR_MemRead _ v')) =
    v == v' && noMemSave (drop (n1 + 1) irH)
            && noMemSave (take n2 irT)
            && noMemSave irM
canElimMemory irH irM irT ((n1, IR_MemSave v _ _)
                          ,(n2, IR_MemSave v' _ _)) =
    v == v' && noMemSave (drop (n1 + 1) irH)
            && noMemSave (take n2 irT)
            && noMemSave irM
canElimMemory irH irM irT ((n1, IR_MemRead x v)
                          ,(n2, IR_MemSave v1 v2 _)) =
    v == v1 && VarIR x == v2
            && noMemSave (drop (n1 + 1) irH)
            && noMemSave (take n2 irT)
            && noMemSave irM
canElimMemory _ _ _ _ = False

doElimMemory :: Int -> BBGraph -> (Ind IR, Ind IR) -> BBGraph
doElimMemory i g ((_, IR_MemRead x _)
                 ,(p, IR_MemRead x' _)) =
    changeBBGraph i p (IR_Ass x' (VarIR x)) g

noMemSave :: [IR] -> Bool
noMemSave = not . (any f)
    where f (IR_MemSave _ _ _) = True
          f _ = False


-- helpers --------------------------------------------------------

allPaths :: BBGraph -> [[Int]]
allPaths g = filter ((>= 2) . length) ps
    where ps :: [[Int]]
          ps = concat (map (List.tails . (getPath g)) (Map.keys (ids g)))

getPath :: BBGraph -> Int -> [Int]
getPath g i = h [] i
    where f = ((prev g) Map.!)
          h acc j = if elem j acc
                       then acc
                       else case f j of
                              [j'] -> h (j:acc) j'
                              _ -> j:acc

pairs :: [a] -> [b] -> [(Ind a, Ind b)]
pairs xs1 xs2 = h (zip [0..] xs1) (zip [0..] xs2)
    where h ys1 ys2 = foldl (\acc y -> (zip (repeat y) ys2) ++ acc) [] ys1

triples :: [a] -> [b] -> [(a, a, Ind b)]
triples xs1 xs2 = h xs1' (zip [0..] xs2)
    where xs1' = map (\((_, a1), (_, a2)) -> (a1, a2)) (pairs xs1 xs1)
          h ys1 ys2 = foldl (\acc y -> (zipWith f (repeat y) ys2) ++ acc) [] ys1
          f (a, b) c = (a, b, c)

middle :: [a] -> [a]
middle = tail . init

getIR :: BBGraph -> Int -> [IR]
getIR g i = f ((ids g) Map.! i)
    where f (BB _ xs) = xs

