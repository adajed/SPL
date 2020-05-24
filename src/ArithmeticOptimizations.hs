module ArithmeticOptimizations where

import IR
import BasicBlock
import OptimizationUtils

arithmeticOptimizations :: BBGraph -> BBGraph
arithmeticOptimizations = mapIR (binOp . condJump)

binOp :: IR -> IR
binOp ir@(IR_BinOp (BOpInt IAdd) x v1 v2) =
    case (v1, v2) of
      (_, VInt 0) -> IR_Ass x v1
      (VInt 0, _) -> IR_Ass x v2
      _ -> if v1 == v2 then IR_BinOp (BOpInt IMul) x v1 (VInt 2) else ir
binOp ir@(IR_BinOp (BOpInt ISub) x v1 v2) =
    case (v1, v2) of
      (_, VInt 0) -> IR_Ass x v1
      (VInt 0, _) -> IR_UnOp (UOpInt INeg) x v2
      _ -> if v1 == v2 then IR_Ass x (VInt 0) else ir
binOp ir@(IR_BinOp (BOpInt IMul) x v1 v2) =
    case (v1, v2) of
      (_, VInt 0) -> IR_Ass x (VInt 0)
      (_, VInt 1) -> IR_Ass x v1
      (_, VInt (-1)) -> IR_UnOp (UOpInt INeg) x v1
      (_, VInt 2) -> IR_BinOp (BOpInt ILshift) x v1 (VInt 1)
      (_, VInt 4) -> IR_BinOp (BOpInt ILshift) x v1 (VInt 2)
      (_, VInt 8) -> IR_BinOp (BOpInt ILshift) x v1 (VInt 3)
      (_, VInt 16) -> IR_BinOp (BOpInt ILshift) x v1 (VInt 4)
      (VInt 0, _) -> IR_Ass x (VInt 0)
      (VInt 1, _) -> IR_Ass x v2
      (VInt (-1), _) -> IR_UnOp (UOpInt INeg) x v2
      (VInt 2, _) -> IR_BinOp (BOpInt ILshift) x v2 (VInt 1)
      (VInt 4, _) -> IR_BinOp (BOpInt ILshift) x v2 (VInt 2)
      (VInt 8, _) -> IR_BinOp (BOpInt ILshift) x v2 (VInt 3)
      (VInt 16, _) -> IR_BinOp (BOpInt ILshift) x v2 (VInt 4)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IDiv) x v1 v2) =
    case (v1, v2) of
      (VInt 0, _) -> IR_Ass x (VInt 0)
      (_, VInt 1) -> IR_Ass x v1
      (_, VInt (-1)) -> IR_UnOp (UOpInt INeg) x v1
      (_, VInt 2) -> IR_BinOp (BOpInt IRshift) x v1 (VInt 1)
      (_, VInt 4) -> IR_BinOp (BOpInt IRshift) x v1 (VInt 2)
      (_, VInt 8) -> IR_BinOp (BOpInt IRshift) x v1 (VInt 3)
      (_, VInt 16) -> IR_BinOp (BOpInt IRshift) x v1 (VInt 4)
      _ -> if v1 == v2 then IR_Ass x (VInt 1) else ir
binOp ir@(IR_BinOp (BOpInt IMod) x v1 v2) =
    case (v1, v2) of
      (_, VInt 1) -> IR_Ass x (VInt 0)
      _ -> if v1 == v2 then IR_Ass x (VInt 0) else ir
binOp ir@(IR_BinOp (BOpInt ILshift) x v1 v2) =
    case (v1, v2) of
      (VInt 0, _) -> IR_Ass x (VInt 0)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IRshift) x v1 v2) =
    case (v1, v2) of
      (VInt 0, _) -> IR_Ass x (VInt 0)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IBitAnd) x v1 v2) =
    case (v1, v2) of
      (VInt 0, _) -> IR_Ass x (VInt 0)
      (_, VInt 0) -> IR_Ass x (VInt 0)
      _ -> ir
binOp ir@(IR_BinOp (BOpBool BAnd) x v1 v2) =
    case (v1, v2) of
      (VBool True, _) -> IR_Ass x v2
      (VBool False, _) -> IR_Ass x (VBool False)
      (_, VBool True) -> IR_Ass x v1
      (_, VBool False) -> IR_Ass x (VBool False)
      _ -> if v1 == v2 then IR_Ass x v1 else ir
binOp ir@(IR_BinOp (BOpBool BOr) x v1 v2) =
    case (v1, v2) of
      (VBool True, _) -> IR_Ass x (VBool True)
      (VBool False, _) -> IR_Ass x v2
      (_, VBool True) -> IR_Ass x (VBool True)
      (_, VBool False) -> IR_Ass x v1
      _ -> if v1 == v2 then IR_Ass x v1 else ir
binOp ir@(IR_BinOp (BOpBool BXor) x v1 v2) =
    case (v1, v2) of
      (VBool True, _) -> IR_UnOp (UOpBool BNot) x v2
      (VBool False, _) -> IR_Ass x v2
      (_, VBool True) -> IR_UnOp (UOpBool BNot) x v1
      (_, VBool False) -> IR_Ass x v1
      _ -> if v1 == v2 then IR_Ass x (VBool False) else ir
binOp ir = ir

condJump :: IR -> IR
condJump ir@(IR_CondJump v1 LTH v2 label) =
    if v1 == v2 then IR_Nop else ir
condJump ir@(IR_CondJump v1 LEQ v2 label) =
    if v1 == v2 then IR_Jump label else ir
condJump ir@(IR_CondJump v1 GTH v2 label) =
    if v1 == v2 then IR_Nop else ir
condJump ir@(IR_CondJump v1 GEQ v2 label) =
    if v1 == v2 then IR_Jump label else ir
condJump ir@(IR_CondJump v1 NEQ v2 label) =
    if v1 == v2 then IR_Nop else ir
condJump ir@(IR_CondJump v1 EQU v2 label) =
    if v1 == v2 then IR_Jump label else ir
condJump ir = ir
