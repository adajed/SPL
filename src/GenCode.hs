module GenCode where

import Data.Map as Map

import CodeM
import IR

import Control.Monad.Trans.State
import Control.Monad.Identity

data SState = SState { code :: [Code]
                     , varAddress :: Map Var Val
                     , offset :: Int
                     , regs :: [Reg]
                     }

type GenCode = StateT SState Identity

execGenCode :: GenCode a -> [Code]
execGenCode m = reverse (code (runIdentity (execStateT m state)))
    where state = SState { code = []
                         , varAddress = Map.empty
                         , offset = 0
                         , regs = [ax, cx, dx, r8, r9, r10, r11]
                         }

emit :: Code -> GenCode ()
emit c = modify (\s -> s { code = c:(code s) } )

getVar :: Var -> GenCode Val
getVar x = do
    m <- gets varAddress
    case (m !? x) of
      Just v -> return v
      Nothing -> fail ("Cannot find " ++ show x)

getFreeReg :: GenCode Reg
getFreeReg = do
    r <- liftM head $ gets regs
    modify (\s -> s { regs = tail (regs s) })
    return r

freeReg :: Reg -> GenCode ()
freeReg r = do
    rs <- gets regs
    unless (elem r rs) (
        modify (\s -> s { regs = r:rs } ))

free :: Val -> GenCode ()
free (VReg reg _) = freeReg reg
free (VMem reg _ _) = freeReg reg
free _ = return ()

toVal :: ValIR -> GenCode Val
toVal (VarIR (SVar var size)) =  getVar var
toVal (IntIR n size) = return (VInt n (getSize size))
toVal (BoolIR True) = return (VInt 1 Byte)
toVal (BoolIR False) = return (VInt 0 Byte)
toVal (LabelIR label) = return (VLabel label)

getMem :: Size -> Val -> GenCode Val
getMem size v@(VMem _ _ _) = do
    reg <- getFreeReg
    emit (CMov (VReg reg QWord) v)
    return (VMem reg 0 size)
getMem size (VReg r _) = return (VMem r 0 size)
getMem s v = return v

getSize :: Int -> Size
getSize 1 = Byte
getSize 2 = Word
getSize 4 = DWord
getSize 8 = QWord
getSize _ = DWord

allocNewVar :: Var -> Int -> GenCode ()
allocNewVar x size = do
    n <- liftM (+size) $ gets offset
    let v = VMem bp (-n) (getSize size)
    modify (\s -> s { varAddress = Map.insert x v (varAddress s)
                    , offset = n })

tryAllocVar :: SVar -> GenCode ()
tryAllocVar (SVar var@(VarA n) size) =
    if n > 6 then return ()
             else (do
                 m <- gets varAddress
                 case (m !? var) of
                   Just _ -> return ()
                   Nothing -> allocNewVar var size)
tryAllocVar (SVar var size) = do
    m <- gets varAddress
    case (m !? var) of
      Just _ -> return ()
      Nothing -> allocNewVar var size

allocValue :: ValIR -> GenCode ()
allocValue (VarIR var) = tryAllocVar var
allocValue _ = return ()

allocVar :: IR -> GenCode ()
allocVar (IR_Ass x v) = do
    tryAllocVar x
    allocValue v
allocVar (IR_BinOp _ x v1 v2) = do
    tryAllocVar x
    allocValue v1
    allocValue v2
allocVar (IR_UnOp _ x v) = do
    tryAllocVar x
    allocValue v
allocVar (IR_MemRead x v) = do
    tryAllocVar x
    allocValue v
allocVar (IR_MemSave v1 v2 _) = do
    allocValue v1
    allocValue v2
allocVar (IR_Call x y xs) = do
    tryAllocVar x
    allocValue y
    mapM_ allocValue xs
allocVar (IR_VoidCall y xs) = do
    allocValue y
    mapM_ allocValue xs
allocVar (IR_CondJump v1 _ v2 _) = do
    allocValue v1
    allocValue v2
allocVar (IR_Return v) = allocValue v
allocVar _ = return ()

eax, ecx, edx :: Val
eax = VReg ax DWord
ecx = VReg cx DWord
edx = VReg dx DWord

rbp, rsp :: Val
rbp = VReg bp QWord
rsp = VReg sp QWord

move :: Val -> Val -> GenCode ()
move v1@(VMem _ _ n1) v2@(VMem _ _ n2) = do
    r <- getFreeReg
    emit (CMov (VReg r n2) v2)
    emit (CMov v1 (VReg r n2))
    freeReg r
move v1 v2 = emit (CMov v1 v2)

moveToReg :: Reg -> Val -> GenCode ()
moveToReg r v@(VMem _ _ s) = emit (CMov (VReg r s) v)
moveToReg r v@(VReg _ s) = emit (CMov (VReg r s) v)
moveToReg r v@(VInt _ s) = emit (CMov (VReg r s) v)


moveToVar :: SVar -> Val -> GenCode ()
moveToVar (SVar var size) v@(VMem _ _ _) = do
    let s = getSize size
    y <- getVar var
    emit (CMov (VReg ax s) v)
    emit (CMov y (VReg ax s))
moveToVar (SVar var size) v = do
    y <- getVar var
    emit (CMov y v)

valIRSize :: ValIR -> Int
valIRSize (VarIR (SVar _ n)) = n
valIRSize (IntIR _ n) = n
valIRSize (BoolIR _) = 1
valIRSize (VoidIR) = 0
valIRSize (LabelIR _) = 8

valSize :: Val -> Size
valSize (VMem _ _ s) = s
valSize (VReg _ s) = s
valSize (VInt _ s) = s

stackOffset :: Int -> Int
stackOffset n = Prelude.mod n' 16
    where n' = 16 - Prelude.mod n 16


genCode :: ([IR], [Int]) -> [Code]
genCode (ir, argSizes) = execGenCode m
    where m = do
              mapM_ allocVar ir
              n <- gets offset
              let n' = n + stackOffset n
              modify (\s -> s { offset = n' })
              emit (CPush rbp)
              emit (CMov rbp rsp)
              emit (CSub rsp (VInt n' QWord))
              foldM_ declareArgFromStack (16, 6) (Prelude.drop 6 argSizes)
              foldM_ declareArgFromArg 1 (Prelude.zip [di, si, dx, cx, r8, r9] (Prelude.take 6 argSizes))
              mapM_ genIR ir

declareArgFromStack :: (Int, Int) -> Int -> GenCode (Int, Int)
declareArgFromStack (n, i) size = do
    let a = VarA n
    let v = VMem bp 16 (getSize size)
    modify (\s -> s { varAddress = Map.insert a v (varAddress s) } )
    return (n + size, i + 1)

declareArgFromArg :: Int -> (Reg, Int) -> GenCode Int
declareArgFromArg i (r, size) = do
    let v = VReg r (getSize size)
    y <- liftM (!(VarA i)) $ gets varAddress
    move y v
    return (i + 1)

genIR :: IR -> GenCode ()
genIR (IR_Label label)  = emit (CLabel (VLabel label))
genIR (IR_Ass x v) = moveToVar x =<< toVal v
genIR (IR_BinOp (BOpInt IAdd) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    let s1 = getSize (valIRSize v1)
    let s2 = getSize (valIRSize v2)
    emit (CMov (VReg ax s1) y1)
    emit (CAdd (VReg ax s2) y2)
    moveToVar x (VReg ax (getSize (valIRSize (VarIR x))))
genIR (IR_BinOp (BOpInt ISub) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    let s1 = getSize (valIRSize v1)
    let s2 = getSize (valIRSize v2)
    emit (CMov (VReg ax s1) y1)
    emit (CSub (VReg ax s2) y2)
    moveToVar x (VReg ax (getSize (valIRSize (VarIR x))))
genIR (IR_BinOp (BOpInt IMul) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CIMul y2)
    moveToVar x eax
genIR (IR_BinOp (BOpInt IDiv) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CIDiv y2)
    moveToVar x eax
genIR (IR_BinOp (BOpInt IMod) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CIDiv y2)
    moveToVar x edx
genIR (IR_BinOp (BOpInt ILshift) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov ecx y2)
    emit (CMov eax y1)
    emit (CShiftL eax (VReg cx Byte))
    moveToVar x eax
genIR (IR_BinOp (BOpInt IRshift) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov ecx y2)
    emit (CMov eax y1)
    emit (CShiftR eax (VReg cx Byte))
    moveToVar x eax
genIR (IR_BinOp (BOpInt IBitAnd) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CBitAnd eax y2)
    moveToVar x eax
genIR (IR_BinOp (BOpInt IBitOr) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CBitOr eax y2)
    moveToVar x eax
genIR (IR_BinOp (BOpInt IBitXor) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CBitXor eax y2)
    moveToVar x eax
genIR (IR_BinOp (BOpBool BAnd) x v1 v2) =
    genIR (IR_BinOp (BOpInt IBitAnd) x v1 v2)
genIR (IR_BinOp (BOpBool BOr) x v1 v2) =
    genIR (IR_BinOp (BOpInt IBitOr) x v1 v2)
genIR (IR_BinOp (BOpBool BXor) x v1 v2) =
    genIR (IR_BinOp (BOpInt IBitXor) x v1 v2)
genIR (IR_UnOp (UOpInt INeg) x v) = do
    y <- toVal v
    emit (CMov eax y)
    emit (CNeg eax)
    moveToVar x eax
genIR (IR_UnOp (UOpInt INot) x v) = do
    y <- toVal v
    emit (CMov eax y)
    emit (CBitNot eax)
    moveToVar x eax
genIR (IR_UnOp (UOpBool BNot) x v) = do
    y <- toVal v
    emit (CMov eax y)
    emit (CBitNot eax)
    moveToVar x eax
genIR (IR_MemRead x v) = do
    y <- toVal v
    let s = getSize (valIRSize (VarIR x))
    emit (CMov (VReg ax QWord) y)
    emit (CMov (VReg ax s) (VMem ax 0 s))
    moveToVar x (VReg ax s)
genIR (IR_MemSave v1 v2 size) = do
    y1 <- getMem (getSize size) =<< toVal v1
    y2 <- toVal v2
    move y1 y2
    free y1
genIR (IR_Call x f xs) = do
    genCall f xs
    let s = getSize (valIRSize (VarIR x))
    moveToVar x (VReg ax s)
genIR (IR_VoidCall f xs) = genCall f xs
genIR (IR_Return v) = do
    let s = getSize (valIRSize v)
    y <- toVal v
    emit (CMov (VReg ax s) y)
    genIR IR_VoidReturn
genIR (IR_VoidReturn) = do
    emit CLeave
    emit CRet
genIR (IR_Jump label) =
    emit (CJump (VLabel label))
genIR (IR_CondJump v1 op v2 label) = do
    y1 <- toVal v1
    y2 <- toVal v2
    let size = valSize y1
    emit (CMov (VReg ax size) y1)
    emit (CCmp (VReg ax size) y2)
    emit (CCondJump op (VLabel label))
genIR _ = emit CNop

genCall :: ValIR -> [ValIR] -> GenCode ()
genCall f xs = do
    let xs_args  = Prelude.take 6 xs
    let xs_stack = Prelude.drop 6 xs
    n <- gets offset
    let i = stackOffset (n + Prelude.sum (Prelude.map valIRSize xs_stack))
    unless (i == 0) (emit (CSub rsp (VInt i QWord)))
    foldM putOnStack 0 xs_stack
    mapM_ putOnArg (Prelude.zip [di, si, dx, cx, r8, r9] xs_args)
    y <- toVal f
    emit (CCall y)
    unless (i == 0) (emit (CAdd rsp (VInt i QWord)))

putOnStack :: Int -> ValIR -> GenCode Int
putOnStack n x = do
    v <- toVal x
    move (VMem sp (-n) (getSize (valIRSize x))) v
    return (n + valIRSize x)

putOnArg :: (Reg, ValIR) -> GenCode ()
putOnArg (r, x) = do
    v <- toVal x
    move (VReg r (getSize (valIRSize x))) v

