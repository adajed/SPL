module IR where

import Data.List ( intercalate )

import AbsSPL ( VIdent )

data Var = VarN VIdent | VarT Int | VarC VIdent Int | VarA Int
    deriving (Eq, Ord)
instance Show Var where
    show (VarN name) = show name
    show (VarT n)    = "t" ++ (show n)
    show (VarC name n) = show name ++ "__" ++ show n
    show (VarA n) = "arg" ++ show n

data SVar = SVar Var Int
    deriving (Eq, Ord)
instance Show SVar where
    show (SVar v size) = show v ++ ":" ++ show size


data ValIR = VarIR SVar
           | IntIR Int Int
           | BoolIR Bool
           | VoidIR
           | LabelIR VIdent
    deriving (Eq)
instance Show ValIR where
    show (VarIR var) = show var
    show (IntIR n size) = show n ++ ":" ++ show size
    show (BoolIR b)  = show b
    show (VoidIR) = "void"
    show (LabelIR label) = show label

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

data IR = IR_Label VIdent                    -- label
        | IR_Ass SVar ValIR                  -- assignment
        | IR_BinOp BinOp SVar ValIR ValIR    -- binary op
        | IR_UnOp UnOp SVar ValIR          -- unary op
        | IR_MemRead SVar ValIR             -- memory read
        | IR_MemSave ValIR ValIR Int        -- memory save
        | IR_Call SVar ValIR [ValIR]         -- call function
        | IR_VoidCall ValIR [ValIR]         -- void call
        | IR_Return ValIR                   -- return value from function
        | IR_VoidReturn
        | IR_Jump VIdent
        | IR_CondJump ValIR RelOp ValIR VIdent
        | IR_Phi SVar [(Int, ValIR)]       -- phi function (for SSA)
        | IR_Nop                          -- no op
        | IR_Argument SVar                 -- argument from function
        deriving (Eq)
instance Show IR where
    show ir = case ir of
                IR_Label name           -> c ["label", show name]
                IR_Ass x v              -> c [show x, "=", show v]
                IR_BinOp op x v1 v2     -> c [show x, "=", show v1, show op, show v2]
                IR_UnOp op x v          -> c [show x, "=", show op, show v]
                IR_MemRead x v          -> c [show x, "=", "*", show v]
                IR_MemSave d s n        -> c ["*", show d, "=", show s, "(", show n, ")"]
                IR_Call y f xs          -> c [show y, "=", "call", show f, show xs]
                IR_VoidCall f xs        -> c ["call", show f, show xs]
                IR_Return v             -> c ["return", show v]
                IR_VoidReturn           -> c ["return"]
                IR_Jump l               -> c ["jump", show l]
                IR_CondJump v1 op v2 l  -> c ["if", show v1, show op, show v2, "jump", show l]
                IR_Phi x vs             -> c [show x, "=", "phi", show vs]
                IR_Nop                  -> c ["nop"]
                IR_Argument x           -> c ["arg", "(", show x, ")"]
            where c = intercalate " "

