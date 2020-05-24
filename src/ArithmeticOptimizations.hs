module ArithmeticOptimizations where

import IR
import BasicBlock
import OptimizationUtils

arithmeticOptimizations :: BBGraph -> BBGraph
arithmeticOptimizations = mapIR (binOp . condJump)

binOp :: IR -> IR
binOp ir@(IR_BinOp (BOpInt IAdd) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0) -> IR_Ass x v1
      (IntIR 0, _) -> IR_Ass x v2
      _ -> if v1 == v2 then IR_BinOp (BOpInt IMul) x v1 (IntIR 2) else ir
binOp ir@(IR_BinOp (BOpInt ISub) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0) -> IR_Ass x v1
      (IntIR 0, _) -> IR_UnOp (UOpInt INeg) x v2
      _ -> if v1 == v2 then IR_Ass x (IntIR 0) else ir
binOp ir@(IR_BinOp (BOpInt IMul) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0) -> IR_Ass x (IntIR 0)
      (_, IntIR 1) -> IR_Ass x v1
      (_, IntIR (-1)) -> IR_UnOp (UOpInt INeg) x v1
      (_, IntIR 2) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 1)
      (_, IntIR 4) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 2)
      (_, IntIR 8) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 3)
      (_, IntIR 16) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 4)
      (IntIR 0, _) -> IR_Ass x (IntIR 0)
      (IntIR 1, _) -> IR_Ass x v2
      (IntIR (-1), _) -> IR_UnOp (UOpInt INeg) x v2
      (IntIR 2, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 1)
      (IntIR 4, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 2)
      (IntIR 8, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 3)
      (IntIR 16, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 4)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IDiv) x v1 v2) =
    case (v1, v2) of
      (IntIR 0, _) -> IR_Ass x (IntIR 0)
      (_, IntIR 1) -> IR_Ass x v1
      (_, IntIR (-1)) -> IR_UnOp (UOpInt INeg) x v1
      (_, IntIR 2) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 1)
      (_, IntIR 4) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 2)
      (_, IntIR 8) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 3)
      (_, IntIR 16) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 4)
      _ -> if v1 == v2 then IR_Ass x (IntIR 1) else ir
binOp ir@(IR_BinOp (BOpInt IMod) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 1) -> IR_Ass x (IntIR 0)
      _ -> if v1 == v2 then IR_Ass x (IntIR 0) else ir
binOp ir@(IR_BinOp (BOpInt ILshift) x v1 v2) =
    case (v1, v2) of
      (IntIR 0, _) -> IR_Ass x (IntIR 0)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IRshift) x v1 v2) =
    case (v1, v2) of
      (IntIR 0, _) -> IR_Ass x (IntIR 0)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IBitAnd) x v1 v2) =
    case (v1, v2) of
      (IntIR 0, _) -> IR_Ass x (IntIR 0)
      (_, IntIR 0) -> IR_Ass x (IntIR 0)
      _ -> ir
binOp ir@(IR_BinOp (BOpBool BAnd) x v1 v2) =
    case (v1, v2) of
      (BoolIR True, _) -> IR_Ass x v2
      (BoolIR False, _) -> IR_Ass x (BoolIR False)
      (_, BoolIR True) -> IR_Ass x v1
      (_, BoolIR False) -> IR_Ass x (BoolIR False)
      _ -> if v1 == v2 then IR_Ass x v1 else ir
binOp ir@(IR_BinOp (BOpBool BOr) x v1 v2) =
    case (v1, v2) of
      (BoolIR True, _) -> IR_Ass x (BoolIR True)
      (BoolIR False, _) -> IR_Ass x v2
      (_, BoolIR True) -> IR_Ass x (BoolIR True)
      (_, BoolIR False) -> IR_Ass x v1
      _ -> if v1 == v2 then IR_Ass x v1 else ir
binOp ir@(IR_BinOp (BOpBool BXor) x v1 v2) =
    case (v1, v2) of
      (BoolIR True, _) -> IR_UnOp (UOpBool BNot) x v2
      (BoolIR False, _) -> IR_Ass x v2
      (_, BoolIR True) -> IR_UnOp (UOpBool BNot) x v1
      (_, BoolIR False) -> IR_Ass x v1
      _ -> if v1 == v2 then IR_Ass x (BoolIR False) else ir
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
