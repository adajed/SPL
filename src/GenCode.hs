module GenCode where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import AbsSPL
import BasicBlock
import GraphColoring ( colorBBGraph )
import CodeM
import IR

import Control.Monad.Trans.State
import Control.Monad.Identity

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb = join (liftM2 f ma mb)

data SState = SState { code          :: [Code]
                     , varAddress    :: M.Map SVar Int
                     , regAllocation :: M.Map SVar Val
                     , offset        :: Int
                     , pushedRegs    :: [Val]
                     , useRbp        :: Bool
                     }

type GenCode = StateT SState Identity

execGenCode :: GenCode a -> [Code]
execGenCode m = reverse (code (runIdentity (execStateT m state)))
    where state = SState { code = []
                         , varAddress = M.empty
                         , regAllocation = M.empty
                         , offset = 0
                         , pushedRegs = []
                         , useRbp = False
                         }

emit :: Code -> GenCode ()
emit c = modify (\s -> s { code = c:(code s) } )

intToSize :: Int -> Size
intToSize 1 = Byte
intToSize 2 = Word
intToSize 4 = DWord
intToSize 8 = QWord

setRegisterAllocation :: M.Map SVar Reg -> GenCode ()
setRegisterAllocation regMap = do
    let allocs = M.mapWithKey (\x@(SVar _ s) r -> VReg r (intToSize s)) regMap
    modify (\s -> s { regAllocation = allocs })

getReg :: SVar -> GenCode Val
getReg x = liftM (M.! x) $ gets regAllocation

getAddress :: SVar -> GenCode Val
getAddress x@(SVar _ s) = do
    b <- gets useRbp
    maybe_n <- liftM (M.!? x) $ gets varAddress
    let size = intToSize s
    case maybe_n of
      Just n ->
          if b
             then return (VMem bp (-n) size)
             else do
                  o <- gets offset
                  return (VMem sp (o-n) size)
      Nothing -> fail ("Cannot find var " ++ show x)

getSpilledVars :: BBGraph -> S.Set SVar
getSpilledVars g = S.unions (map go (M.elems (ids g)))
    where go (BB _ xs) = S.unions (map go' xs)
          go' (IR_Store x) = S.singleton x
          go' _ = S.empty

allocVarOnStack :: SVar -> GenCode ()
allocVarOnStack v@(SVar (VarA n) s) =
    if n > 6 then return ()
             else do
                  n <- liftM (+s) $ gets offset
                  modify (\s -> s { varAddress = M.insert v n (varAddress s)
                                  , offset = n })
allocVarOnStack v@(SVar _ s) = do
    n <- liftM (+s) $ gets offset
    modify (\s -> s { varAddress = M.insert v n (varAddress s)
                    , offset = n })

alignTo16 :: Int -> Int
alignTo16 n = mod (16 - mod n 16) 16

stackOffset :: Int -> GenCode Int
stackOffset n = do
    b <- gets useRbp
    if b then return (alignTo16 n)
         else return (alignTo16 (n + 8))

-- generate code

genCode :: BBGraph -> [Code]
genCode g = execGenCode (genBBGraph g)

genBBGraph :: BBGraph -> GenCode ()
genBBGraph g = do
    let (g', regAlloc) = colorBBGraph g
    setRegisterAllocation regAlloc
    let inds = flattenBBGraph g'
    let bbs = map ((ids g') M.!) inds
    mapM_ allocVarOnStack (getSpilledVars g')
    n <- gets offset
    when (n > 0) (do
                  emit (CPush rbp)
                  emit (CMov rbp rsp)
                  let n' = n + alignTo16 n
                  modify (\s -> s { useRbp = True
                                  , offset = n' })
                  emit (CSub rsp (VInt n' QWord))
                 )
    let usedCallerSaveRegs = S.filter (`elem` callerSaveRegs) (S.fromList (M.elems regAlloc))
    mapM_ (push . (`VReg` QWord)) usedCallerSaveRegs
    foldM_ declareArgFromStack (0, 7) (drop 6 (args g'))
    mapM_ genBasicBlock bbs

moveArgToReg :: SVar -> Val -> GenCode ()
moveArgToReg v@(SVar (VarA n) s) r =
    if n <= 6
       then do
            let r1 = VReg (argRegs L.!! (n-1)) (intToSize s)
            move r r1
        else do
            addr <- getAddress v
            move r addr

genBasicBlock :: BasicBlock -> GenCode ()
genBasicBlock (BB (VIdent ".__START__") xs) = do
    allocs <- gets regAllocation
    let isArg (SVar (VarA _) _) _ = True
        isArg _ _ = False
    let argVars = M.filterWithKey isArg allocs
    mapM_ (uncurry moveArgToReg) (M.toList argVars)
    mapM_ genIR xs
genBasicBlock (BB label xs) = do
    emit (CLabel (VLabel label))
    mapM_ genIR xs

declareArgFromStack :: (Int, Int) -> Int -> GenCode (Int, Int)
declareArgFromStack (n, i) s = do
    let a = (SVar (VarA i) s)
    b <- gets useRbp
    let n' = if b then (-16 - n) else (-8 - n)
    modify (\st -> st { varAddress = M.insert a n' (varAddress st) })
    return (n + 8, i + 1)

toVal :: ValIR -> GenCode Val
toVal (IntIR n s) = return (VInt n (intToSize s))
toVal (LabelIR l) = return (VLabel l)
toVal (VarIR x)   = getReg x

move :: Val -> Val -> GenCode ()
move v1 v2 = when (v1 /= v2) (emit (CMov v1 v2))

push :: Val -> GenCode ()
push v@(VInt n _) = do
    modify (\s -> s { offset = (offset s) + 8
                    , pushedRegs = v:(pushedRegs s) })
    emit (CPush (VInt n DWord))
push v@(VReg r _) = do
    modify (\s -> s { offset = (offset s) + 8
                    , pushedRegs = v:(pushedRegs s) })
    emit (CPush (VReg r QWord))
push v@(VLabel _) = do
    modify (\s -> s { offset = (offset s) + 8
                    , pushedRegs = v:(pushedRegs s) })
    emit (CPush v)

pop :: Val -> GenCode ()
pop v = do
    modify (\s -> s { offset = (offset s) - sizeToInt (takeSize v) })
    emit (CPop v)

leave :: GenCode ()
leave = do
    regs <- gets pushedRegs
    mapM_ (emit . CPop) regs
    b <- gets useRbp
    when b (emit CLeave)

toVMem :: Val -> Size -> Val
toVMem (VReg r _) size = VMem r 0 size

setSize :: Size -> Val -> Val
setSize size (VReg r _) = VReg r size
setSize size (VInt n _) = VInt n size

genIR_BinOp :: (Val -> Val -> Code) -> SVar -> ValIR -> ValIR -> GenCode ()
genIR_BinOp f y v1 v2 = do
    r_y <- getReg y
    r_v1 <- toVal v1
    r_v2 <- toVal v2
    if r_y == r_v1
        then emit (f r_y r_v2)
    else if r_y == r_v2
        then emit (f r_y r_v1)
    else do
         move r_y r_v1
         emit (f r_y r_v2)

genIR_UnOp :: (Val -> Code) -> SVar -> ValIR -> GenCode ()
genIR_UnOp f y v = do
    liftJoin2 move (getReg y) (toVal v)
    emit =<< liftM f (getReg y)

genIR :: IR -> GenCode ()

-- label
genIR (IR_Label label) = emit (CLabel (VLabel label))

-- assignement
genIR (IR_Ass y v) = liftJoin2 move (getReg y) (toVal v)

-- add
genIR (IR_BinOp IAdd y v1 v2) = do
    r_y <- getReg y
    let size = takeSize r_y
    r_v1 <- liftM (setSize size) $ toVal v1
    r_v2 <- liftM (setSize size) $ toVal v2
    if r_y == r_v1
        then emit (CAdd r_y r_v2)
    else if r_y == r_v2
        then emit (CAdd r_y r_v1)
    else do
         move r_y r_v1
         emit (CAdd r_y r_v2)

-- sub
genIR (IR_BinOp ISub y v1 v2) = do
    r_y <- getReg y
    r_v1 <- toVal v1
    r_v2 <- toVal v2
    if r_y == r_v1
        then emit (CSub r_y r_v2)
    else do
         move r_y r_v1
         emit (CSub r_y r_v2)

-- mul
genIR (IR_BinOp IMul y v1 v2) =
    genIR_BinOp CIMul y v1 v2

-- div
genIR (IR_BinOp IDiv y v1 v2) = do
    r_y <- getReg y
    r_v1 <- toVal v1
    r_v2 <- toVal v2
    let a = VReg ax (takeSize r_v1)
    move a r_v1
    emit CCdq
    emit (CIDiv r_v2)
    move r_y a

-- mod
genIR (IR_BinOp IMod y v1 v2) = do
    r_y <- getReg y
    r_v1 <- toVal v1
    r_v2 <- toVal v2
    let a = VReg ax (takeSize r_v1)
    move a r_v1
    emit CCdq
    emit (CIDiv r_v2)
    let d = VReg dx (takeSize r_y)
    move r_y d

-- lshift
genIR (IR_BinOp ILshift y v (IntIR n s)) = do
    r_y <- getReg y
    r_v <- toVal v
    move r_y r_v
    emit (CShiftL r_y (VInt n Byte))

genIR (IR_BinOp ILshift y v1 v2) = do
    r_y <- getReg y
    r_v1 <- toVal v1
    r_v2 <- toVal v2
    move r_y r_v1
    let c = VReg cx (takeSize r_v2)
    move c r_v2
    emit (CShiftL r_y c)

-- rshift
genIR (IR_BinOp IRshift y v (IntIR n s)) = do
    r_y <- getReg y
    r_v <- toVal v
    move r_y r_v
    emit (CShiftR r_y (VInt n Byte))

genIR (IR_BinOp IRshift y v1 v2) = do
    r_y <- getReg y
    r_v1 <- toVal v1
    r_v2 <- toVal v2
    move r_y r_v1
    let c = VReg cx (takeSize r_v2)
    move c r_v2
    emit (CShiftR r_y c)

-- bitand
genIR (IR_BinOp IBitAnd y v1 v2) =
    genIR_BinOp CBitAnd y v1 v2

-- bitor
genIR (IR_BinOp IBitOr y v1 v2) =
    genIR_BinOp CBitOr y v1 v2

-- bitxor
genIR (IR_BinOp IBitXor y v1 v2) =
    genIR_BinOp CBitXor y v1 v2

-- and
-- or
-- xor

-- neg
genIR (IR_UnOp INeg y v) =
    genIR_UnOp CNeg y v

-- bitnot
genIR (IR_UnOp IBitNot y v) =
    genIR_UnOp CBitNot y v
-- not

-- memory read
genIR (IR_MemRead x v) = do
    r_x <- getReg x
    r_v <- liftM (`toVMem` (takeSize r_x)) $ toVal v
    move r_x r_v

-- memory save
genIR (IR_MemSave v1 v2 s) = do
    let size = intToSize s
    liftJoin2 move (liftM (`toVMem` size) (toVal v1)) (toVal v2)

-- call
genIR (IR_Call y f xs) = do
    genIR (IR_VoidCall f xs)
    r <- getReg y
    let a = VReg ax (takeSize r)
    move r a

-- void call
genIR (IR_VoidCall f xs) = do
    let h v r = do
                let r' = VReg r (intToSize (valIRSize v))
                v' <- toVal v
                move r' v'
    mapM_ (uncurry h) (zip xs argRegs)
    let stackArgs = drop 6 xs
    offset_ <- gets offset
    let newoffset = offset_ + 8 * (length stackArgs)
    align <- stackOffset newoffset
    when (align > 0) (do
                      emit (CSub rsp (VInt align QWord))
                      modify (\s -> s { offset = (offset s) + align })
                     )
    mapM_ (push <=< toVal) (reverse stackArgs)
    emit =<< liftM CCall (toVal f)
    let n = align + 8 * length stackArgs
    when (n > 0) (do
                  emit (CAdd rsp (VInt n QWord))
                  modify (\s -> s { offset = (offset s) + n
                                  , pushedRegs = drop (length stackArgs) (pushedRegs s) })
                 )


-- jump
genIR (IR_Jump label) = emit (CJump (VLabel label))

-- conditional jump
genIR (IR_CondJump v1 op v2 label) = do
    emit =<< liftM2 CCmp (toVal v1) (toVal v2)
    emit (CCondJump op (VLabel label))

-- return
genIR (IR_Return v) = do
    let a = VReg ax (intToSize (valIRSize v))
    liftJoin2 move (return a) (toVal v)
    genIR IR_VoidReturn

-- void return
genIR IR_VoidReturn = do
    leave
    emit CRet

-- store
genIR (IR_Store x) =
    emit =<< liftM2 CMov (getAddress x) (getReg x)

-- load
genIR (IR_Load x) =
    emit =<< liftM2 CMov (getReg x) (getAddress x)

-- nop
genIR IR_Nop = emit CNop


genIR _ = emit CNop
