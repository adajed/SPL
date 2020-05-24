module OptimizationUtils where

import Data.Map as Map

import BasicBlock
import IR

modifyValue :: (ValIR -> ValIR) -> IR -> IR
modifyValue f ir =
    case ir of
      IR_Ass x v -> IR_Ass x (f v)
      IR_BinOp op x v1 v2 -> IR_BinOp op x (f v1) (f v2)
      IR_UnOp op x v -> IR_UnOp op x (f v)
      IR_MemRead x v -> IR_MemRead x (f v)
      IR_MemSave v1 v2 -> IR_MemSave (f v1) (f v2)
      IR_Call x v xs -> IR_Call x (f v) (fmap f xs)
      IR_VoidCall v xs -> IR_VoidCall (f v) (fmap f xs)
      IR_Return v -> IR_Return (f v)
      IR_CondJump v1 op v2 l -> IR_CondJump (f v1) op (f v2) l
      IR_Phi x vs -> IR_Phi x (Prelude.map (\(n,v) -> (n, f v)) vs)
      ir' -> ir'

takeVar :: IR -> Maybe Var
takeVar (IR_Ass x _) = Just x
takeVar (IR_BinOp _ x _ _) = Just x
takeVar (IR_UnOp _ x _) = Just x
takeVar (IR_MemRead x _) = Just x
takeVar (IR_Call x _ _) = Just x
takeVar (IR_Phi x _) = Just x
takeVar _ = Nothing

mapIR :: (IR -> IR) -> BBGraph -> BBGraph
mapIR f g = g { ids = Map.map (\(BB name xs) -> BB name (Prelude.map f xs)) (ids g) }

liftBB :: ([IR] -> [IR]) -> BasicBlock -> BasicBlock
liftBB f (BB name xs) = BB name (f xs)
