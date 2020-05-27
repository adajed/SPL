module ArithmeticOptimizations where

import IR
import BasicBlock
import OptimizationUtils

arithmeticOptimizations :: BBGraph -> BBGraph
arithmeticOptimizations = mapIR (binOp . condJump)

binOp :: IR -> IR
binOp ir@(IR_BinOp (BOpInt IAdd) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0 _) -> IR_Ass x v1
      (IntIR 0 _, _) -> IR_Ass x v2
      _ -> if v1 == v2 then IR_BinOp (BOpInt IMul) x v1 (IntIR 2 4) else ir
binOp ir@(IR_BinOp (BOpInt ISub) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0 _) -> IR_Ass x v1
      (IntIR 0 _, _) -> IR_UnOp (UOpInt INeg) x v2
      _ -> if v1 == v2 then IR_Ass x (IntIR 0 4) else ir
binOp ir@(IR_BinOp (BOpInt IMul) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0 n) -> IR_Ass x (IntIR 0 n)
      (_, IntIR 1 _) -> IR_Ass x v1
      (_, IntIR (-1) _) -> IR_UnOp (UOpInt INeg) x v1
      (_, IntIR 2 n) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 1 n)
      (_, IntIR 4 n) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 2 n)
      (_, IntIR 8 n) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 3 n)
      (_, IntIR 16 n) -> IR_BinOp (BOpInt ILshift) x v1 (IntIR 4 n)
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      (IntIR 1 _, _) -> IR_Ass x v2
      (IntIR (-1) _, _) -> IR_UnOp (UOpInt INeg) x v2
      (IntIR 2 n, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 1 n)
      (IntIR 4 n, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 2 n)
      (IntIR 8 n, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 3 n)
      (IntIR 16 n, _) -> IR_BinOp (BOpInt ILshift) x v2 (IntIR 4 n)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IDiv) x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      (_, IntIR 1 _) -> IR_Ass x v1
      (_, IntIR (-1) _) -> IR_UnOp (UOpInt INeg) x v1
      (_, IntIR 2 n) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 1 n)
      (_, IntIR 4 n) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 2 n)
      (_, IntIR 8 n) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 3 n)
      (_, IntIR 16 n) -> IR_BinOp (BOpInt IRshift) x v1 (IntIR 4 n)
      _ -> if v1 == v2 then IR_Ass x (IntIR 1 4) else ir
binOp ir@(IR_BinOp (BOpInt IMod) x v1 v2) =
    case (v1, v2) of
      (_, IntIR 1 n) -> IR_Ass x (IntIR 0 n)
      _ -> if v1 == v2 then IR_Ass x (IntIR 0 4) else ir
binOp ir@(IR_BinOp (BOpInt ILshift) x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IRshift) x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      _ -> ir
binOp ir@(IR_BinOp (BOpInt IBitAnd) x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      (_, IntIR 0 n) -> IR_Ass x (IntIR 0 n)
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
