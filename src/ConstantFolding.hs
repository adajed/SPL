module ConstantFolding where

import Data.Bits as Bits
import Data.Map as Map

import BasicBlock
import IR
import OptimizationUtils

constantFolding :: BBGraph -> BBGraph
constantFolding = mapIR constantFold

constantFold :: IR -> IR
constantFold (IR_BinOp op x (IntIR n1 s1) (IntIR n2 s2)) =
    IR_Ass x (IntIR (calcBinOp op n1 n2) (max s1 s2))
constantFold (IR_UnOp op x (IntIR n s)) =
    IR_Ass x (IntIR (calcUnOp op n) s)
constantFold ir@(IR_CondJump v1 EQU v2 label) = if v1 == v2 then IR_Jump label else ir
constantFold (IR_CondJump (IntIR n1 _) op (IntIR n2 _) label) =
    if calcIRelOp op n1 n2 then IR_Jump label else IR_Nop
constantFold ir = ir

intToBool :: Int -> Bool
intToBool n = n /= 0

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

calcBinOp :: BinOp -> Int -> Int -> Int
calcBinOp IAdd n1 n2       = n1 + n2
calcBinOp ISub n1 n2       = n1 - n2
calcBinOp IMul n1 n2       = n1 * n2
calcBinOp IDiv n1 n2       = n1 `quot` n2
calcBinOp IMod n1 n2       = n1 `mod` n2
calcBinOp ILshift n1 n2    = n1 `Bits.shiftL` n2
calcBinOp IRshift n1 n2    = n1 `Bits.shiftR` n2
calcBinOp IBitAnd n1 n2    = n1 .&. n2
calcBinOp IBitOr n1 n2     = n1 .|. n2
calcBinOp IBitXor n1 n2    = n1 `Bits.xor` n2
calcBinOp BAnd n1 n2       = boolToInt ((intToBool n1) && (intToBool n2))
calcBinOp BOr n1 n2        = boolToInt ((intToBool n1) || (intToBool n2))
calcBinOp BXor n1 n2       = boolToInt ((intToBool n1) /= (intToBool n2))

calcUnOp :: UnOp -> Int -> Int
calcUnOp INeg n    = negate n
calcUnOp IBitNot n = Bits.complement n
calcUnOp BNot n    = boolToInt (not (intToBool n))

calcIRelOp :: RelOp -> Int -> Int -> Bool
calcIRelOp LTH = (<)
calcIRelOp LEQ = (<=)
calcIRelOp GTH = (>)
calcIRelOp GEQ = (>=)
calcIRelOp EQU = (==)
calcIRelOp NEQ = (/=)
