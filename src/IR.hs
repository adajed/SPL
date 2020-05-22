module IR where

import Data.List ( intercalate )

import AbsSPL ( Ident )

data Var = VarN Ident | VarT Int | VarC Ident Int
    deriving (Eq, Ord)
instance Show Var where
    show (VarN name) = show name
    show (VarT n)    = "t" ++ (show n)
    show (VarC name n) = show name ++ "__" ++ show n

data ValIR = VarIR Var
           | IntIR Int
           | BoolIR Bool
           | VoidIR
           | LabelIR Ident
           | ArgIR Int
    deriving (Eq)
instance Show ValIR where
    show (VarIR var) = show var
    show (IntIR n) = show n
    show (BoolIR b)  = show b
    show (VoidIR) = "void"
    show (LabelIR label) = show label
    show (ArgIR n) = "arg" ++ show n

data IBinOp = IAdd | ISub | IMul | IDiv | IMod | ILshift | IRshift | IBitAnd | IBitOr | IBitXor
    deriving (Eq)
instance Show IBinOp where
    show IAdd    = "+"
    show ISub    = "-"
    show IMul    = "*"
    show IDiv    = "/"
    show IMod    = "%"
    show ILshift = "<<"
    show IRshift = ">>"
    show IBitAnd = "&"
    show IBitOr  = "|"
    show IBitXor = "^"

data IUnOp = INeg | INot
    deriving (Eq)
instance Show IUnOp where
    show INeg = "-"
    show INot = "~"

data BBinOp = BAnd | BOr | BXor
    deriving (Eq)
instance Show BBinOp where
    show BAnd = "and"
    show BOr  = "or"
    show BXor = "xor"

data BUnOp = BNot
    deriving (Eq)
instance Show BUnOp where
    show BNot = "not"

data RelOp = LTH | LEQ | GTH | GEQ | EQU | NEQ
    deriving (Eq)
instance Show RelOp where
    show LTH = "<"
    show LEQ = "<="
    show GTH = ">"
    show GEQ = ">="
    show EQU = "=="
    show NEQ = "!="

data BinOp = BOpInt IBinOp | BOpBool BBinOp | BOpRel RelOp
    deriving (Eq)
instance Show BinOp where
    show (BOpInt op) = show op
    show (BOpBool op) = show op
    show (BOpRel op) = show op

data UnOp = UOpInt IUnOp | UOpBool BUnOp
    deriving (Eq)
instance Show UnOp where
    show (UOpInt op) = show op
    show (UOpBool op) = show op

data IR = IR_Label Ident                    -- label
        | IR_Ass Var ValIR                  -- assignment
        | IR_BinOp BinOp Var ValIR ValIR    -- binary op
        | IR_UnOp UnOp Var ValIR          -- unary op
        | IR_MemRead Var ValIR           -- memory read
        | IR_MemSave ValIR ValIR            -- memory save
        | IR_Param ValIR                    -- set next param to function
        | IR_Call Var ValIR Int             -- call function
        | IR_Return ValIR                   -- return value from function
        | IR_Jump Ident
        | IR_CondJump ValIR RelOp ValIR Ident
        | IR_Phi Var [(Int, ValIR)]       -- phi function (for SSA)
        | IR_Nop                          -- no op
        | IR_Argument Var                 -- argument from function
        deriving (Eq)
instance Show IR where
    show ir = case ir of
                IR_Label name           -> c ["label", show name]
                IR_Ass x v              -> c [show x, "=", show v]
                IR_BinOp op x v1 v2     -> c [show x, "=", show v1, show op, show v2]
                IR_UnOp op x v          -> c [show x, "=", show op, show v]
                IR_MemRead x v          -> c [show x, "=", "*", show v]
                IR_MemSave d s          -> c ["*", show d, "=", show s]
                IR_Param v              -> c ["param", show v]
                IR_Call x f n           -> c [show x, "=", "call", show f, show n]
                IR_Return v             -> c ["return", show v]
                IR_Jump l               -> c ["jump", show l]
                IR_CondJump v1 op v2 l  -> c ["if", show v1, show op, show v2, "jump", show l]
                IR_Phi x vs              -> c [show x, "=", "phi", show vs]
                IR_Nop                  -> c ["nop"]
                IR_Argument x           -> c ["arg", "(", show x, ")"]
            where c = intercalate " "

