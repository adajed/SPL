module ConstantFolding where

import Data.Bits as Bits
import Data.Map as Map

import BasicBlock
import IR
import OptimizationUtils

constantFolding :: BBGraph -> BBGraph
constantFolding = mapIR constantFold

constantFold :: IR -> IR
constantFold (IR_IBinOp op x (VInt n1) (VInt n2)) =
    IR_Ass x (VInt (calcIBinOp op n1 n2))
constantFold (IR_BBinOp op x (VBool b1) (VBool b2)) =
    IR_Ass x (VBool (calcBBinOp op b1 b2))
constantFold (IR_IRelOp op x (VInt n1) (VInt n2)) =
    IR_Ass x (VBool (calcIRelOp op n1 n2))
constantFold (IR_IUnOp op x (VInt n)) =
    IR_Ass x (VInt (calcIUnOp op n))
constantFold (IR_BUnOp op x (VBool b)) =
    IR_Ass x (VBool (calcBUnOp op b))
constantFold (IR_CondJump (VBool True) label) = IR_Jump label
constantFold (IR_CondJump (VBool False) _) = IR_Nop
constantFold ir = ir

calcIBinOp :: IBinOp -> Int -> Int -> Int
calcIBinOp IAdd = (+)
calcIBinOp ISub = (-)
calcIBinOp IMul = (*)
calcIBinOp IDiv = quot
calcIBinOp IMod = mod
calcIBinOp ILshift = Bits.shiftL
calcIBinOp IRshift = Bits.shiftR
calcIBinOp IAnd = (.&.)
calcIBinOp IOr = (.|.)
calcIBinOp IXor = Bits.xor

calcIUnOp :: IUnOp -> Int -> Int
calcIUnOp INeg = negate
calcIUnOp INot = Bits.complement

calcBBinOp :: BBinOp -> Bool -> Bool -> Bool
calcBBinOp BAnd = (&&)
calcBBinOp BOr = (||)
calcBBinOp BXor = (/=)

calcBUnOp :: BUnOp -> Bool -> Bool
calcBUnOp BNot = not

calcIRelOp :: IRelOp -> Int -> Int -> Bool
calcIRelOp ILT = (<)
calcIRelOp ILE = (<=)
calcIRelOp IGT = (>)
calcIRelOp IGE = (>=)
calcIRelOp IEQ = (==)
calcIRelOp INEQ = (/=)
