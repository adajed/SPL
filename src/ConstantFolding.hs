module ConstantFolding where

import Data.Bits as Bits
import Data.Map as Map

import BasicBlock
import IR
import OptimizationUtils

constantFolding :: BBGraph -> BBGraph
constantFolding = mapIR constantFold

constantFold :: IR -> IR
constantFold (IR_BinOp (BOpInt op) x (IntIR n1) (IntIR n2)) =
    IR_Ass x (IntIR (calcIBinOp op n1 n2))
constantFold (IR_BinOp (BOpBool op) x (BoolIR b1) (BoolIR b2)) =
    IR_Ass x (BoolIR (calcBBinOp op b1 b2))
constantFold (IR_UnOp (UOpInt op) x (IntIR n)) =
    IR_Ass x (IntIR (calcIUnOp op n))
constantFold (IR_UnOp (UOpBool op) x (BoolIR b)) =
    IR_Ass x (BoolIR (calcBUnOp op b))
constantFold ir@(IR_CondJump v1 EQU v2 label) = if v1 == v2 then IR_Jump label else ir
constantFold (IR_CondJump (IntIR n1) op (IntIR n2) label) =
    if calcIRelOp op n1 n2 then IR_Jump label else IR_Nop
constantFold ir = ir

calcIBinOp :: IBinOp -> Int -> Int -> Int
calcIBinOp IAdd = (+)
calcIBinOp ISub = (-)
calcIBinOp IMul = (*)
calcIBinOp IDiv = quot
calcIBinOp IMod = mod
calcIBinOp ILshift = Bits.shiftL
calcIBinOp IRshift = Bits.shiftR
calcIBinOp IBitAnd = (.&.)
calcIBinOp IBitOr = (.|.)
calcIBinOp IBitXor = Bits.xor

calcIUnOp :: IUnOp -> Int -> Int
calcIUnOp INeg = negate
calcIUnOp INot = Bits.complement

calcBBinOp :: BBinOp -> Bool -> Bool -> Bool
calcBBinOp BAnd = (&&)
calcBBinOp BOr = (||)
calcBBinOp BXor = (/=)

calcBUnOp :: BUnOp -> Bool -> Bool
calcBUnOp BNot = not

calcIRelOp :: RelOp -> Int -> Int -> Bool
calcIRelOp LTH = (<)
calcIRelOp LEQ = (<=)
calcIRelOp GTH = (>)
calcIRelOp GEQ = (>=)
calcIRelOp EQU = (==)
calcIRelOp NEQ = (/=)
