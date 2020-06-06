module CodeM where

import AbsSPL
import IR

import Data.List ( intercalate )

data Size = Byte | Word | DWord | QWord
    deriving (Eq, Ord)
instance Show Size where
    show Byte  = "BYTE"
    show Word  = "WORD"
    show DWord = "DWORD"
    show QWord = "QWORD"

data Reg = RegB Char   -- (rax, rbx, rcx, rdx)
         | RegS String -- (rdi, rsi, rbp, rsp)
         | RegN Int    -- (r8, ... , r15)
         deriving (Eq, Ord)
instance Show Reg where
    show (RegB c) = [c, 'x']
    show (RegS s) = s
    show (RegN n) = "r" ++ show n

ax, bx, cx, dx :: Reg
ax = RegB 'a'
bx = RegB 'b'
cx = RegB 'c'
dx = RegB 'd'

di, si, bp, sp :: Reg
di = RegS "di"
si = RegS "si"
bp = RegS "bp"
sp = RegS "sp"

r8, r9, r10, r11, r12, r13, r14, r15 :: Reg
r8  = RegN 8
r9  = RegN 9
r10 = RegN 10
r11 = RegN 11
r12 = RegN 12
r13 = RegN 13
r14 = RegN 14
r15 = RegN 15


showReg :: Size -> Reg -> String
showReg Byte  (RegB c) = [c, 'l']
showReg Word  (RegB c) = [c, 'x']
showReg DWord (RegB c) = ['e', c, 'x']
showReg QWord (RegB c) = ['r', c, 'x']
showReg Byte  (RegS s) = s ++ "b"
showReg Word  (RegS s) = s
showReg DWord (RegS s) = 'e':s
showReg QWord (RegS s) = 'r':s
showReg Byte  (RegN n) = "r" ++ show n ++ "b"
showReg Word  (RegN n) = "r" ++ show n ++ "w"
showReg DWord (RegN n) = "r" ++ show n ++ "d"
showReg QWord (RegN n) = "r" ++ show n


data Val = VInt Int Size
         | VReg Reg Size
         | VMem Reg Int Size
         | VLabel VIdent
         deriving (Eq, Ord)
instance Show Val where
    show (VInt n size)   = show size ++ " " ++ show n
    show (VLabel l)      = show l
    show (VReg r size)   = showReg size r
    show (VMem r n size) = show size ++ " [" ++ showReg QWord r ++ f n ++ "]"
        where f x | x == 0    = ""
                  | x > 0     = "+" ++ show x
                  | otherwise = show x

isInt :: Val -> Bool
isInt (VInt _ _) = True
isInt _ = False

isReg :: Val -> Bool
isReg (VReg _ _) = True
isReg _ = False

isMem :: Val -> Bool
isMem (VMem _ _ _) = True
isMem _ = False

isLabel :: Val -> Bool
isLabel (VLabel _) = True
isLabel _ = False

takeSize :: Val -> Size
takeSize (VInt _ s) = s
takeSize (VReg _ s) = s
takeSize (VMem _ _ s) = s
takeSize (VLabel _) = QWord

data Code = CAdd Val Val    -- add
          | CBitAnd Val Val -- bitwise and
          | CCall Val       -- call
          | CCmp Val Val    -- compare
          | CDec Val        -- decrement
          | CUDiv Val       -- unsigned div
          | CIDiv Val       -- signed div
          | CIMul Val Val   -- signed mul
          | CInc Val        -- increment
          | CJump Val       -- jump
          | CCondJump IR.RelOp Val -- conditional jump
          | CMov Val Val    -- move
          | CNeg Val        -- neg
          | CBitNot Val     -- bitwise not
          | CBitOr Val Val  -- bitwise or
          | CLabel Val
          | CPop Val
          | CPush Val
          | CShiftL Val Val
          | CShiftR Val Val
          | CSub Val Val
          | CBitXor Val Val    -- bitwise xor
          | CNop
          | CLeave
          | CRet               -- return
          deriving (Eq)
instance Show Code where
    show code = case code of
                  CAdd a b              -> c ["add", show a, ",", show b]
                  CBitAnd a b           -> c ["and", show a, ",", show b]
                  CCall a               -> c ["call", show a]
                  CCmp a b              -> c ["cmp", show a, ",", show b]
                  CDec a                -> c ["dec", show a]
                  CUDiv a               -> c ["div", show a]
                  CIDiv a               -> c ["idiv", show a]
                  CIMul a b             -> c ["imul", show a, ",", show b]
                  CInc a                -> c ["inc", show a]
                  CJump a               -> c ["jmp", show a]
                  CCondJump IR.LTH a    -> c ["jl", show a]
                  CCondJump IR.LEQ a    -> c ["jle", show a]
                  CCondJump IR.GTH a    -> c ["jg", show a]
                  CCondJump IR.GEQ a    -> c ["jge", show a]
                  CCondJump IR.NEQ a    -> c ["jne", show a]
                  CCondJump IR.EQU a    -> c ["je", show a]
                  CMov a b              -> c ["mov", show a, ",", show b]
                  CNeg a                -> c ["neg", show a]
                  CBitNot a             -> c ["not", show a]
                  CBitOr a b            -> c ["or", show a, ",", show b]
                  CLabel a              -> c [show a, ":"]
                  CPop a                -> c ["pop", show a]
                  CPush a               -> c ["push", show a]
                  CShiftL a b           -> c ["sal", show a, ",", show b]
                  CShiftR a b           -> c ["sar", show a, ",", show b]
                  CSub a b              -> c ["sub", show a, ",", show b]
                  CBitXor a b           -> c ["xor", show a, ",", show b]
                  CNop                  -> c ["nop"]
                  CLeave                -> c ["leave"]
                  CRet                  -> c ["ret"]
                where c = intercalate " "
