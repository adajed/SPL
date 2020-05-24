module GenCode where

import Data.Map as Map

import CodeM
import IR

import Control.Monad.Trans.State
import Control.Monad.Identity

data SState = SState { code :: [Code]
                     , varAddress :: Map Var Val
                     , offset :: Int
                     , argCounter :: Int
                     }

type GenCode = StateT SState Identity

execGenCode :: GenCode a -> [Code]
execGenCode m = reverse (code (runIdentity (execStateT m state)))
    where state = SState { code = []
                         , varAddress = Map.empty
                         , offset = 0
                         , argCounter }

emit :: Code -> GenCode ()
emit c = modify (\s -> s { code = c:(code s) } )

getVar :: Var -> GenCode Val
getVar x = do
    m <- gets varAddress
    case (m !? x) of
      Just v -> return v
      Nothing -> fail ("Cannot find " ++ show x)

toVal :: ValIR -> GenCode Val
toVal (VarIR x) =  getVar x
toVal (IntIR n) = return (VInt n DWord)
toVal (BoolIR True) = return (VInt 1 DWord)
toVal (BoolIR False) = return (VInt 0 DWord)
toVal (LabelIR label) = return (VLabel label)
toVal (ArgIR _) = return (VReg ax DWord)

varToVal :: Var -> Val
varToVal _ = reg

getMem :: Val -> GenCode Val
getMem v@(VMem _ _ _) = do
    emit (CMov eax v)
    return (VMem ax 0 DWord)
getMem (VReg r size) = return (VMem r 0 size)
getMem v = return v

reg :: Val
reg = VReg ax DWord

tryAllocVar :: Var -> GenCode ()
tryAllocVar x = do
    m <- gets varAddress
    case (m !? x) of
      Just _ -> return ()
      Nothing -> (do
                 n <- gets offset
                 let v = VMem sp (-n) DWord
                 let m' = Map.insert x v m
                 modify (\s -> s { varAddress = m', offset = n + 4 }))

allocValue :: ValIR -> GenCode ()
allocValue (VarIR x) = tryAllocVar x
allocValue (ArgIR n) = tryAllocVar (VarA n)
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
allocVar (IR_MemSave v1 v2) = do
    allocValue v1
    allocValue v2
allocVar (IR_Param v) = allocValue v
allocVar (IR_Call x y _) = do
    tryAllocVar x
    allocValue y
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

moveToVar :: Var -> Val -> GenCode ()
moveToVar x v@(VMem _ _ _) = do
    y <- getVar x
    emit (CMov eax v)
    emit (CMov y eax)
moveToVar x v = do
    y <- getVar x
    emit (CMov y v)

genCode :: [IR] -> [Code]
genCode ir = execGenCode m
    where m = do
              mapM_ allocVar ir
              n <- gets offset
              emit (CPush rbp)
              emit (CMov rbp rsp)
              emit (CSub rsp (VInt n QWord))
              mapM_ genIR ir

genIR :: IR -> GenCode ()
genIR (IR_Label label)  = emit (CLabel (VLabel label))
genIR (IR_Ass x v) = moveToVar x =<< toVal v
genIR (IR_BinOp (BOpInt IAdd) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CAdd eax y2)
    moveToVar x eax
genIR (IR_BinOp (BOpInt ISub) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CSub eax y2)
    moveToVar x eax
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
    emit (CShiftL eax ecx)
    moveToVar x eax
genIR (IR_BinOp (BOpInt IRshift) x v1 v2) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov ecx y2)
    emit (CMov eax y1)
    emit (CShiftR eax ecx)
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
genIR (IR_BinOp (BOpRel LTH) x v1 v2) = genIR_cmp v1 v2
genIR (IR_BinOp (BOpRel LEQ) x v1 v2) = genIR_cmp v1 v2
genIR (IR_BinOp (BOpRel GTH) x v1 v2) = genIR_cmp v1 v2
genIR (IR_BinOp (BOpRel GEQ) x v1 v2) = genIR_cmp v1 v2
genIR (IR_BinOp (BOpRel EQU) x v1 v2) = genIR_cmp v1 v2
genIR (IR_BinOp (BOpRel NEQ) x v1 v2) = genIR_cmp v1 v2
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
    y <- getMem =<< toVal v
    moveToVar x y
genIR (IR_MemSave v1 v2) = do
    y1 <- getMem =<< toVal v1
    y2 <- toVal v2
    emit (CMov y1 y2)
genIR (IR_Param v) = do
    y <- toVal v
    emit (CMov (VReg di DWord) y)
genIR (IR_Call x v _) = do
    y <- toVal v
    emit (CCall y)
    moveToVar x eax


genIR (IR_Return v) = do
    y <- toVal v
    emit (CMov eax y)
    emit CLeave
    emit CRet

genIR (IR_Jump label) =
    emit (CJump (VLabel label))
genIR (IR_CondJump v1 op v2 label) = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CCmp eax y2)
    emit (CCondJump op (VLabel label))
genIR _ = emit CNop

genIR_cmp :: ValIR -> ValIR -> GenCode ()
genIR_cmp v1 v2 = do
    y1 <- toVal v1
    y2 <- toVal v2
    emit (CMov eax y1)
    emit (CCmp eax y2)

