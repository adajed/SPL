module LocalCommonSubexpressionElimination where

import Data.Map as Map

import IR
import BasicBlock

import OptimizationUtils

localCommonSubexpressionElimination :: BBGraph -> BBGraph
localCommonSubexpressionElimination g = g { ids = Map.map f (ids g) }
    where f = liftBB (localCommonSubexprElim . memElim)

localCommonSubexprElim :: [IR] -> [IR]
localCommonSubexprElim xs = Prelude.foldl elim xs c
    where c = Prelude.filter isCommonSubExpr (allPairs xs)
          elim xs ((n1, ir1), (n2, ir2)) =
              case (takeVar ir1, takeVar ir2) of
                (Just x, Just y) -> change n2 (IR_Ass y (VarIR x)) xs
                _ -> xs

memElim :: [IR] -> [IR]
memElim xs = Prelude.foldl eliminate xs c
    where c = Prelude.filter (isCommonMem xs) (allPairs xs)

isCommonMem :: [IR] -> ((Int, IR), (Int, IR)) -> Bool
isCommonMem xs ((n1, IR_MemRead _ v),   (n2, IR_MemRead _ v')) = v == v' && noMemSave xs n1 n2
isCommonMem xs ((n1, IR_MemSave v _ _), (n2, IR_MemRead _ v')) = v == v' && noMemSave xs n1 n2
isCommonMem xs ((n1, IR_MemSave v _ _), (n2, IR_MemSave v' _ _)) = v == v' && noMemSave xs n1 n2
isCommonMem _ _ = False

noMemSave :: [IR] -> Int -> Int -> Bool
noMemSave xs n1 n2 = h (Prelude.take (n2 - n1 - 1) (Prelude.drop (n1+1) xs))
    where h [] = True
          h ((IR_MemSave _ _ _):_) = False
          h ((IR_Call _ _ _):_) = False
          h ((IR_VoidCall _ _):_) = False
          h (y:ys) = h ys

eliminate :: [IR] -> ((Int, IR), (Int, IR)) -> [IR]
eliminate xs ((n1, IR_MemRead x _), (n2, IR_MemRead y _)) = change n2 newir xs
    where newir = IR_Ass y (VarIR x)
eliminate xs ((n1, IR_MemSave _ _ _), (n2, IR_MemSave _ _ _)) = change n1 IR_Nop xs
eliminate xs ((n1, IR_MemSave _ v _), (n2, IR_MemRead x _)) = change n2 newir xs
    where newir = IR_Ass x v
eliminate xs _ = xs

change :: Int -> a -> [a] -> [a]
change n x xs = (Prelude.take n xs) ++ [x] ++ (Prelude.drop (n+1) xs)

allPairs :: [a] -> [((Int, a), (Int, a))]
allPairs xs = h (zip [0..] xs) []
    where h :: [(Int, a)] -> [((Int, a), (Int, a))] -> [((Int, a), (Int, a))]
          h [] acc = acc
          h (y:ys) acc = h ys ((zip (repeat y) ys) ++ acc)

isCommonSubExpr :: ((Int, IR), (Int, IR)) -> Bool
isCommonSubExpr ((_, IR_BinOp op _ v1 v2), (_, IR_BinOp op' _ v1' v2')) =
    op == op' && v1 == v1' && v2 == v2'
isCommonSubExpr ((_, IR_UnOp op _ v), (_, IR_UnOp op' _ v')) = op == op' && v == v'
isCommonSubExpr _ = False
