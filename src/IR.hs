module IR where

import Data.List ( intercalate )

import AbsSPL ( Ident )

data Var = VarN Ident | VarT Int | VarC Ident Int | VarA Int
    deriving (Eq, Ord)
instance Show Var where
    show (VarN name) = show name
    show (VarT n)    = "t" ++ (show n)
    show (VarC name n) = show name ++ "__" ++ show n
    show (VarA n) = "arg" ++ show n

data Value = VVar Var
           | VInt Int
           | VBool Bool
           | VVoid
           | VLabel Ident
           | VArg Int
    deriving (Eq)
instance Show Value where
    show (VVar var) = show var
    show (VInt n) = show n
    show (VBool b)  = show b
    show (VVoid) = "void"
    show (VLabel label) = show label
    show (VArg n) = "arg" ++ show n

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

data BinOp = BOpInt IBinOp | BOpBool BBinOp
    deriving (Eq)
instance Show BinOp where
    show (BOpInt op) = show op
    show (BOpBool op) = show op

data UnOp = UOpInt IUnOp | UOpBool BUnOp
    deriving (Eq)
instance Show UnOp where
    show (UOpInt op) = show op
    show (UOpBool op) = show op

data IR = IR_Label Ident                    -- label
        | IR_Ass Var Value                  -- assignment
        | IR_BinOp BinOp Var Value Value    -- binary op
        | IR_UnOp UnOp Var Value          -- unary op
        | IR_MemRead Var Value           -- memory read
        | IR_MemSave Value Value            -- memory save
        | IR_Call Var Value [Value]         -- call function
        | IR_VoidCall Value [Value]         -- void call
        | IR_Return Value                   -- return value from function
        | IR_Jump Ident
        | IR_CondJump Value RelOp Value Ident
        | IR_Phi Var [(Int, Value)]       -- phi function (for SSA)
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
                IR_Call y f xs          -> c [show y, "=", "call", show f, show xs]
                IR_VoidCall f xs        -> c ["call", show f, show xs]
                IR_Return v             -> c ["return", show v]
                IR_Jump l               -> c ["jump", show l]
                IR_CondJump v1 op v2 l  -> c ["if", show v1, show op, show v2, "jump", show l]
                IR_Phi x vs              -> c [show x, "=", "phi", show vs]
                IR_Nop                  -> c ["nop"]
                IR_Argument x           -> c ["arg", "(", show x, ")"]
            where c = intercalate " "

