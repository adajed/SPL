module DeadCodeElimination (
    deadCodeElimination
    ) where

import Data.Map as Map

import AbsSPL
import BasicBlock
import IR
import OptimizationUtils

deadCodeElimination :: BBGraph -> BBGraph
deadCodeElimination g = mapIR f g
    where f = (changeToNopIfUnused g)
            . (callToVoidCall g)
            . trivialCopy

changeToNopIfUnused :: BBGraph -> IR -> IR
changeToNopIfUnused g ir =
    if isUsed g ir then ir else IR_Nop

callToVoidCall :: BBGraph -> IR -> IR
callToVoidCall g ir@(IR_Call y f xs) =
    if isVarUsed g y then ir else IR_VoidCall f xs
callToVoidCall g ir = ir

trivialCopy :: IR -> IR
trivialCopy ir@(IR_Ass x v) =
    if VarIR x == v then IR_Nop else ir
trivialCopy ir = ir

lastN :: Int -> [a] -> [a]
lastN n xs = Prelude.drop (length xs - n) xs


isUsed :: BBGraph -> IR -> Bool
isUsed g ir =
    case ir of
      IR_Ass x _ -> isVarUsed g x
      IR_BinOp _ x _ _ -> isVarUsed g x
      IR_UnOp _ x _ -> isVarUsed g x
      IR_MemRead x _ -> isVarUsed g x
      IR_Phi x _ -> isVarUsed g x
      ir' -> True

isVarUsed :: BBGraph -> SVar -> Bool
isVarUsed g x = any f (Map.elems (ids g))
    where f (BB _ xs) = any (uses (VarIR x)) xs

uses :: ValIR -> IR -> Bool
uses v ir =
    case ir of
      IR_Label _            -> False
      IR_Ass _ v'           -> v == v'
      IR_BinOp _ _ v1 v2    -> v1 == v || v2 == v
      IR_UnOp _ _ v'        -> v == v'
      IR_MemRead _ v'       -> v == v'
      IR_MemSave v1 v2 _    -> v1 == v || v2 == v
      IR_Call _ v' vs'      -> v == v' || any (==v) vs'
      IR_VoidCall v' vs'    -> v == v' || any (==v) vs'
      IR_Return v'          -> v == v'
      IR_VoidReturn         -> False
      IR_Jump _             -> False
      IR_CondJump v1 _ v2 _ _ -> v1 == v || v2 == v
      IR_Phi _ vs           -> any ((==v) . snd) vs
      IR_Nop                -> False

