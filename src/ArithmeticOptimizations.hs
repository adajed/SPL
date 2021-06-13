module ArithmeticOptimizations where

import IR
import BasicBlock
import OptimizationUtils
import Operator

arithmeticOptimizations :: BBGraph -> BBGraph
arithmeticOptimizations = mapIR (binOp . condJump)

binOp :: IR -> IR
binOp ir@(IR_BinOp IAdd x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0 _) -> IR_Ass x v1
      (IntIR 0 _, _) -> IR_Ass x v2
      _ -> if v1 == v2 then IR_BinOp IMul x v1 (IntIR 2 4) else ir
binOp ir@(IR_BinOp ISub x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0 _) -> IR_Ass x v1
      (IntIR 0 _, _) -> IR_UnOp INeg x v2
      _ -> if v1 == v2 then IR_Ass x (IntIR 0 4) else ir
binOp ir@(IR_BinOp IMul x v1 v2) =
    case (v1, v2) of
      (_, IntIR 0 n) -> IR_Ass x (IntIR 0 n)
      (_, IntIR 1 _) -> IR_Ass x v1
      (_, IntIR (-1) _) -> IR_UnOp INeg x v1
      (_, IntIR 2 n) -> IR_BinOp ILshift x v1 (IntIR 1 n)
      (_, IntIR 4 n) -> IR_BinOp ILshift x v1 (IntIR 2 n)
      (_, IntIR 8 n) -> IR_BinOp ILshift x v1 (IntIR 3 n)
      (_, IntIR 16 n) -> IR_BinOp ILshift x v1 (IntIR 4 n)
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      (IntIR 1 _, _) -> IR_Ass x v2
      (IntIR (-1) _, _) -> IR_UnOp INeg x v2
      (IntIR 2 n, _) -> IR_BinOp ILshift x v2 (IntIR 1 n)
      (IntIR 4 n, _) -> IR_BinOp ILshift x v2 (IntIR 2 n)
      (IntIR 8 n, _) -> IR_BinOp ILshift x v2 (IntIR 3 n)
      (IntIR 16 n, _) -> IR_BinOp ILshift x v2 (IntIR 4 n)
      _ -> ir
binOp ir@(IR_BinOp IDiv x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      (_, IntIR 1 _) -> IR_Ass x v1
      (_, IntIR (-1) _) -> IR_UnOp INeg x v1
      (_, IntIR 2 n) -> IR_BinOp IRshift x v1 (IntIR 1 n)
      (_, IntIR 4 n) -> IR_BinOp IRshift x v1 (IntIR 2 n)
      (_, IntIR 8 n) -> IR_BinOp IRshift x v1 (IntIR 3 n)
      (_, IntIR 16 n) -> IR_BinOp IRshift x v1 (IntIR 4 n)
      _ -> if v1 == v2 then IR_Ass x (IntIR 1 4) else ir
binOp ir@(IR_BinOp IMod x v1 v2) =
    case (v1, v2) of
      (_, IntIR 1 n) -> IR_Ass x (IntIR 0 n)
      _ -> if v1 == v2 then IR_Ass x (IntIR 0 4) else ir
binOp ir@(IR_BinOp ILshift x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      _ -> ir
binOp ir@(IR_BinOp IRshift x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      _ -> ir
binOp ir@(IR_BinOp IBitAnd x v1 v2) =
    case (v1, v2) of
      (IntIR 0 n, _) -> IR_Ass x (IntIR 0 n)
      (_, IntIR 0 n) -> IR_Ass x (IntIR 0 n)
      _ -> ir
binOp ir@(IR_BinOp BAnd x v1 v2) =
    case (v1, v2) of
      (IntIR 1 1, _) -> IR_Ass x v2
      (IntIR 0 1, _) -> IR_Ass x (IntIR 0 1)
      (_, IntIR 1 1) -> IR_Ass x v1
      (_, IntIR 0 1) -> IR_Ass x (IntIR 0 1)
      _ -> if v1 == v2 then IR_Ass x v1 else ir
binOp ir@(IR_BinOp BOr x v1 v2) =
    case (v1, v2) of
      (IntIR 1 1, _) -> IR_Ass x (IntIR 1 1)
      (IntIR 0 1, _) -> IR_Ass x v2
      (_, IntIR 1 1) -> IR_Ass x (IntIR 1 1)
      (_, IntIR 0 1) -> IR_Ass x v1
      _ -> if v1 == v2 then IR_Ass x v1 else ir
binOp ir@(IR_BinOp BXor x v1 v2) =
    case (v1, v2) of
      (IntIR 1 1, _) -> IR_UnOp BNot x v2
      (IntIR 0 1, _) -> IR_Ass x v2
      (_, IntIR 1 1) -> IR_UnOp BNot x v1
      (_, IntIR 0 1) -> IR_Ass x v1
      _ -> if v1 == v2 then IR_Ass x (IntIR 0 1) else ir
binOp ir = ir

condJump :: IR -> IR
condJump ir@(IR_CondJump v1 Less v2 label1 label2) =
    if v1 == v2 then IR_Jump label2 else ir
condJump ir@(IR_CondJump v1 LessEq v2 label1 label2) =
    if v1 == v2 then IR_Jump label1 else ir
condJump ir@(IR_CondJump v1 Greater v2 label1 label2) =
    if v1 == v2 then IR_Jump label2 else ir
condJump ir@(IR_CondJump v1 GreaterEq v2 label1 label2) =
    if v1 == v2 then IR_Jump label1 else ir
condJump ir@(IR_CondJump v1 NotEqual v2 label1 label2) =
    if v1 == v2 then IR_Jump label2 else ir
condJump ir@(IR_CondJump v1 Equal v2 label1 label2) =
    if v1 == v2 then IR_Jump label1 else ir
condJump ir = ir
