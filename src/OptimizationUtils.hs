module OptimizationUtils where

import Data.Map as Map

import BasicBlock
import IR

modifyValue :: (Value -> Value) -> IR -> IR
modifyValue f ir =
    case ir of
      IR_Ass x v -> IR_Ass x (f v)
      IR_IBinOp op x v1 v2 -> IR_IBinOp op x (f v1) (f v2)
      IR_BBinOp op x v1 v2 -> IR_BBinOp op x (f v1) (f v2)
      IR_IRelOp op x v1 v2 -> IR_IRelOp op x (f v1) (f v2)
      IR_IUnOp op x v -> IR_IUnOp op x (f v)
      IR_BUnOp op x v -> IR_BUnOp op x (f v)
      IR_MemRead x v -> IR_MemRead x (f v)
      IR_MemSave v1 v2 -> IR_MemSave (f v1) (f v2)
      IR_Param v -> IR_Param (f v)
      IR_Call x v n -> IR_Call x (f v) n
      IR_Return v -> IR_Return (f v)
      IR_CondJump v l -> IR_CondJump (f v) l
      IR_Phi x vs -> IR_Phi x (Prelude.map (\(n,v) -> (n, f v)) vs)
      ir' -> ir'

mapIR :: (IR -> IR) -> BBGraph -> BBGraph
mapIR f g = g { ids = Map.map (\(BB name xs) -> BB name (Prelude.map f xs)) (ids g) }
