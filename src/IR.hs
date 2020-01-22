module IR where

import Data.List ( intercalate )

import AbsSPL ( Ident )

data Var = VarN Ident | VarT Int
instance Show Var where
    show (VarN name) = show name
    show (VarT n)    = "t" ++ (show n)

data Value = VVar Var
           | VInt Integer
           | VBool Bool
           | VVoid
           | VLabel Ident
instance Show Value where
    show (VVar var) = show var
    show (VInt n) = show n
    show (VBool b)  = show b
    show (VVoid) = "void"
    show (VLabel label) = show label

data IBinOp = IAdd | ISub | IMul | IDiv | IMod | ILshift | IRshift | IAnd | IOr | IXor
instance Show IBinOp where
    show IAdd    = "+"
    show ISub    = "-"
    show IMul    = "*"
    show IDiv    = "/"
    show IMod    = "%"
    show ILshift = "<<"
    show IRshift = ">>"
    show IAnd    = "&"
    show IOr     = "|"
    show IXor    = "^"

data IUnOp = INeg | INot
instance Show IUnOp where
    show INeg = "-"
    show INot = "~"

data BBinOp = BAnd | BOr | BXor
instance Show BBinOp where
    show BAnd = "and"
    show BOr  = "or"
    show BXor = "xor"

data BUnOp = BNot
instance Show BUnOp where
    show BNot = "not"

data IRelOp = ILT | ILE | IGT | IGE | IEQ | INEQ
instance Show IRelOp where
    show ILT = "<"
    show ILE = "<="
    show IGT = ">"
    show IGE = ">="
    show IEQ = "=="
    show INEQ = "!="


data IR = IR_Label Ident                    -- label
        | IR_Ass Var Value                  -- assignment
        | IR_IBinOp IBinOp Var Value Value  -- binary op on integers
        | IR_IUnOp IUnOp Var Value          -- unary op on intergers
        | IR_BBinOp BBinOp Var Value Value  -- binary op on bools
        | IR_BUnOp BUnOp Var Value          -- unary op on bools
        | IR_IRelOp IRelOp Var Value Value  -- relation op on intergers
        | IR_Memory Var Value               -- memory address
        | IR_Param Value                    -- set next param to function
        | IR_Call Var Value Int             -- call function
        | IR_Return Value                   -- return value from function
        | IR_Jump Ident
        | IR_CondJump Value Ident
instance Show IR where
    show ir = case ir of
                IR_Label name        -> c ["label", show name]
                IR_Ass x v           -> c [show x, "=", show v]
                IR_IBinOp op x v1 v2 -> c [show x, "=", show v1, show op, show v2]
                IR_IUnOp op x v      -> c [show x, "=", show op, show v]
                IR_BBinOp op x v1 v2 -> c [show x, "=", show v1, show op, show v2]
                IR_BUnOp op x v      -> c [show x, "=", show op, show v]
                IR_IRelOp op x v1 v2 -> c [show x, "=", show v1, show op, show v2]
                IR_Param v           -> c ["param", show v]
                IR_Call x f n        -> c [show x, "=", "call", show f, show n]
                IR_Return v          -> c ["return", show v]
                IR_Jump name         -> c ["jump", show name]
                IR_CondJump v name   -> c ["if", show v, "jump", show name]
            where c = intercalate " "

