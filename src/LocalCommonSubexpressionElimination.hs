module LocalCommonSubexpressionElimination where

import qualified Data.Map as Map

import IR
import BasicBlock

import OptimizationUtils

localCommonSubexpressionElimination :: BBGraph -> BBGraph
localCommonSubexpressionElimination g = g { ids = Map.map f (ids g) }
    where f = liftBB (elimMemory . elimTriples . elimPairs)

-- eliminate pairs

elimPairs :: [IR] -> [IR]
elimPairs xs = foldl doElimPairs xs c
    where c = filter canElimPair (allPairs xs)

canElimPair :: (Ind IR, Ind IR) -> Bool
canElimPair ((_, IR_BinOp op  _ v1  v2)
            ,(_, IR_BinOp op' _ v1' v2')) =
    op == op' && v1 == v1' && v2 == v2'
canElimPair ((_, IR_UnOp op  _ v)
            ,(_, IR_UnOp op' _ v')) =
    op == op' && v == v'
canElimPair _ = False

doElimPairs :: [IR] -> (Ind IR, Ind IR) -> [IR]
doElimPairs xs ((_, IR_BinOp _ x _ _)
               ,(p, IR_BinOp _ y _ _)) =
    change p (IR_Ass y (VarIR x)) xs
doElimPairs xs ((_, IR_UnOp _ x _)
               ,(p, IR_UnOp _ y _)) =
    change p (IR_Ass y (VarIR x)) xs
doElimPairs xs _ = xs

-- eliminate triples

elimTriples :: [IR] -> [IR]
elimTriples xs = foldl doElimTriple xs c
    where c = filter canElimTriple (allTriples xs)

canElimTriple :: (Ind IR, Ind IR, Ind IR) -> Bool
canElimTriple ((_, IR_BinOp IMul x1 v1 (IntIR n1 _)),
               (_, IR_BinOp IAdd x2 v2 (IntIR n2 _)),
               (_, IR_BinOp IMul x3 v3 (IntIR n3 _))) =
    v1 == v2 && (VarIR x2) == v3 && n1 == n3
canElimTriple ((_, IR_BinOp ILshift x1 v1 (IntIR n1 _)),
               (_, IR_BinOp IAdd x2 v2 (IntIR n2 _)),
               (_, IR_BinOp ILshift x3 v3 (IntIR n3 _))) =
    v1 == v2 && (VarIR x2) == v3 && n1 == n3
canElimTriple ((_, IR_BinOp IAdd x1 v1 w1),
               (_, IR_BinOp IAdd x2 v2 w2),
               (_, IR_BinOp IAdd x3 v3 w3)) =
    v1 == v3 && w1 == v2 && VarIR x2 == w3
canElimTriple _ = False

doElimTriple :: [IR] -> (Ind IR, Ind IR, Ind IR) -> [IR]
doElimTriple xs ((_, IR_BinOp IMul x1 v1 (IntIR n1 s)),
                 (_, IR_BinOp IAdd x2 v2 (IntIR n2 _)),
                 (n3, IR_BinOp IMul x3 v3 _)) =
    change n3 (IR_BinOp IAdd x3 (VarIR x1) (IntIR (n1 * n2) s)) xs
doElimTriple xs ((_, IR_BinOp ILshift x1 v1 (IntIR n1 s)),
                 (_, IR_BinOp IAdd x2 v2 (IntIR n2 _)),
                 (n3, IR_BinOp ILshift x3 v3 _)) =
    change n3 (IR_BinOp IAdd x3 (VarIR x1) (IntIR ((2 ^ n1) * n2) s)) xs
doElimTriple xs ((_, IR_BinOp IAdd x1 v1 w1),
                 (_, IR_BinOp IAdd x2 v2 w2),
                 (n3, IR_BinOp IAdd x3 v3 w3)) =
    change n3 (IR_BinOp IAdd x3 (VarIR x1) w2) xs
doElimTriple xs _ = xs


-- eliminate memory

elimMemory :: [IR] -> [IR]
elimMemory xs = Prelude.foldl doElimMemory xs c
    where c = Prelude.filter (canElimMemory xs) (allPairs xs)

canElimMemory :: [IR] -> ((Int, IR), (Int, IR)) -> Bool
canElimMemory xs ((n1, IR_MemRead _ v)
                 ,(n2, IR_MemRead _ v')) =
    v == v' && noMemSave xs n1 n2
canElimMemory xs ((n1, IR_MemSave v _ _)
                 ,(n2, IR_MemRead _ v')) =
    v == v' && noMemSave xs n1 n2
canElimMemory xs ((n1, IR_MemSave v _ _)
                 ,(n2, IR_MemSave v' _ _)) =
    v == v' && noMemSave xs n1 n2
canElimMemory xs ((n1, IR_MemRead x v)
                 ,(n2, IR_MemSave v1 v2 _)) =
    v == v1 && VarIR x == v2 && noMemSave xs n1 n2
canElimMemory _ _ = False

noMemSave :: [IR] -> Int -> Int -> Bool
noMemSave xs n1 n2 = h (Prelude.take (n2 - n1 - 1) (Prelude.drop (n1+1) xs))
    where h [] = True
          h ((IR_MemSave _ _ _):_) = False
          h ((IR_Call _ _ _):_) = False
          h ((IR_VoidCall _ _):_) = False
          h (y:ys) = h ys

doElimMemory :: [IR] -> (Ind IR, Ind IR) -> [IR]
doElimMemory xs ((_, IR_MemRead x _)
                ,(p, IR_MemRead y _)) =
    change p (IR_Ass y (VarIR x)) xs
doElimMemory xs ((p, IR_MemSave _ _ _)
                ,(_, IR_MemSave _ _ _)) =
    change p IR_Nop xs
doElimMemory xs ((_, IR_MemSave _ v _)
                ,(p, IR_MemRead x _)) =
    change p (IR_Ass x v) xs
doElimMemory xs ((_, IR_MemRead _ _)
                ,(p, IR_MemSave _ _ _)) =
    change p IR_Nop xs
doElimMemory xs _ = xs
