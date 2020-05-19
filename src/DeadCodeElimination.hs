module DeadCodeElimination where

import Data.Map as Map

import BasicBlock
import IR
import OptimizationUtils

deadCodeElimination :: BBGraph -> BBGraph
deadCodeElimination g = mapIR (changeToNopIfUnused g) g

changeToNopIfUnused :: BBGraph -> IR -> IR
changeToNopIfUnused g ir =
    if isUsed g ir then ir else IR_Nop

isUsed :: BBGraph -> IR -> Bool
isUsed g ir =
    case ir of
      IR_Ass x _ -> isVarUsed g x
      IR_IBinOp _ x _ _ -> isVarUsed g x
      IR_BBinOp _ x _ _ -> isVarUsed g x
      IR_IRelOp _ x _ _ -> isVarUsed g x
      IR_IUnOp _ x _ -> isVarUsed g x
      IR_BUnOp _ x _ -> isVarUsed g x
      IR_MemRead x _ -> isVarUsed g x
      IR_Phi x _ -> isVarUsed g x
      ir' -> True

isVarUsed :: BBGraph -> Var -> Bool
isVarUsed g x = any f (Map.elems (ids g))
    where f (BB _ xs) = any (uses (VVar x)) xs

uses :: Value -> IR -> Bool
uses v ir =
    case ir of
      IR_Ass _ v' -> v == v'
      IR_IBinOp _ _ v1 v2 -> v1 == v || v2 == v
      IR_BBinOp _ _ v1 v2 -> v1 == v || v2 == v
      IR_IRelOp _ _ v1 v2 -> v1 == v || v2 == v
      IR_IUnOp _ _ v' -> v == v'
      IR_BUnOp _ _ v' -> v == v'
      IR_MemRead _ v' -> v == v'
      IR_MemSave v1 v2 -> v1 == v || v2 == v
      IR_Param v' -> v == v'
      IR_Call _ v' _ -> v == v'
      IR_Return v' -> v == v'
      IR_CondJump v' _ -> v == v'
      IR_Phi _ vs -> any ((==v) . snd) vs
      ir' -> False

