module LocalCommonSubexpressionElimination where

import Data.Map as Map

import IR
import BasicBlock

import OptimizationUtils

localCommonSubexpressionElimination :: BBGraph -> BBGraph
localCommonSubexpressionElimination g = g { ids = Map.map f (ids g) }
    where f = liftBB (localCommonSubexprElim . memReadElim)

localCommonSubexprElim :: [IR] -> [IR]
localCommonSubexprElim xs = Prelude.foldl eliminate xs c
    where c = Prelude.filter isCommonSubExpr (allPairs xs)

memReadElim :: [IR] -> [IR]
memReadElim xs = Prelude.foldl eliminate xs c
    where c = Prelude.filter (isCommonMemRead xs) (allPairs xs)

isCommonMemRead :: [IR] -> (IR, IR) -> Bool
isCommonMemRead xs (ir1@(IR_MemRead _ v), ir2@(IR_MemRead _ v')) = v == v' && noMemSave xs ir1 ir2
isCommonMemRead _ _ = False

noMemSave :: [IR] -> IR -> IR -> Bool
noMemSave xs x1 x2 = h xs 0
    where h [] _ = False
          h (y:ys) 0 = h ys (if x1 == y then 1 else 0)
          h ((IR_MemSave _ _):ys) 1 = False
          h (y:ys) 1 = if y == x2 then True else h ys 1

eliminate :: [IR] -> (IR, IR) -> [IR]
eliminate xs (ir1, ir2) =
    case (takeVar ir1, takeVar ir2) of
      (Just x, Just y) ->
          let newir = IR_Ass y (VarIR x)
              f ir = if ir == ir2 then newir else ir
           in Prelude.map f xs
      _ -> xs


allPairs :: [a] -> [(a, a)]
allPairs xs = h xs []
    where h [] acc = acc
          h (y:ys) acc = h ys ((zip (repeat y) ys) ++ acc)

isCommonSubExpr :: (IR, IR) -> Bool
isCommonSubExpr (IR_BinOp op _ v1 v2, IR_BinOp op' _ v1' v2') =
    op == op' && v1 == v1' && v2 == v2'
isCommonSubExpr (IR_UnOp op _ v, IR_UnOp op' _ v') = op == op' && v == v'
isCommonSubExpr _ = False
