module Operator (
    Operator(..),
    unary,
    binary
    ) where

data Operator = Neg
              | Not
              | BitNot
              | Or
              | And
              | Plus
              | Minus
              | Times
              | Div
              | Mod
              | LShift
              | RShift
              | BitAnd
              | BitOr
              | BitXor
              | Less
              | LessEq
              | Greater
              | GreaterEq
              | Equal
              | NotEqual
    deriving (Eq, Ord, Read)

instance Show Operator where
    show Neg        = "-"
    show Not        = "!"
    show BitNot     = "~"
    show Or         = "||"
    show And        = "&&"
    show Plus       = "+"
    show Minus      = "-"
    show Times      = "*"
    show Div        = "/"
    show Mod        = "%"
    show LShift     = "<<"
    show RShift     = ">>"
    show BitAnd     = "&"
    show BitOr      = "|"
    show BitXor     = "^"
    show Less       = "<"
    show LessEq     = "<="
    show Greater    = ">"
    show GreaterEq  = ">="
    show Equal      = "=="
    show NotEqual   = "!="

unary :: Operator -> Bool
unary op = op == Neg || op == Not || op == BitNot

binary :: Operator -> Bool
binary = not . unary
