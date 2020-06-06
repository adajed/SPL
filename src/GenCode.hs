module GenCode where

import qualified Data.Map as M
import qualified Data.Set as S

import BasicBlock
import CalculateLiveVars
import CodeM
import IR

import Control.Monad.Trans.State
import Control.Monad.Identity

data SState = SState { code :: [Code]
                     , varAddress :: M.Map SVar Val
                     , varCurrentLocations :: M.Map SVar (S.Set Val)
                     , regsContent :: M.Map Reg (S.Set SVar)
                     , offset :: Int
                     }

type GenCode = StateT SState Identity

regs, argRegs :: [Reg]
regs = [ax, cx, dx, di, si, r8, r9, r10, r11]
argRegs = [di, si, dx, cx, r8, r9]

execGenCode :: GenCode a -> [Code]
execGenCode m = reverse (code (runIdentity (execStateT m state)))
    where state = SState { code = []
                         , varAddress = M.empty
                         , varCurrentLocations = M.empty
                         , regsContent = M.fromList (zip regs (repeat S.empty))
                         , offset = 0
                         }

emit :: Code -> GenCode ()
emit c = modify (\s -> s { code = c:(code s) } )

getVarAddress :: SVar -> GenCode Val
getVarAddress x = liftM (M.! x) $ gets varAddress

getVarLocations :: SVar -> GenCode (S.Set Val)
getVarLocations x = liftM (M.! x) $ gets varCurrentLocations

modifyVarLocations :: SVar -> (S.Set Val -> S.Set Val) -> GenCode ()
modifyVarLocations x f = modify (\s -> s { varCurrentLocations = M.adjust f x (varCurrentLocations s) })

modifyRegContent :: Reg -> (S.Set SVar -> S.Set SVar) -> GenCode ()
modifyRegContent r f = modify (\s -> s { regsContent = M.adjust f r (regsContent s) })

setVarLocations :: SVar -> S.Set Val -> GenCode ()
setVarLocations x vals = do
    mapM_ (`addVarToVal` x) vals
    modifyVarLocations x (const vals)

clearVarLocations :: SVar -> GenCode ()
clearVarLocations x = do
    locs <- getVarLocations x
    mapM_ (`clearFromVal` x) locs
    setVarLocations x S.empty

clearReg :: Reg -> GenCode ()
clearReg r = do
    modifyRegContent r (const S.empty)
    let f (VReg r' _) = r /= r'
        f _ = True
    modify (\s -> s { varCurrentLocations = M.map (S.filter f) (varCurrentLocations s) })

clearVar :: SVar -> GenCode ()
clearVar x = do
    modifyVarLocations x (const S.empty)
    modify (\s -> s { regsContent = M.map (S.delete x) (regsContent s) })

clearFromVal :: Val -> SVar -> GenCode ()
clearFromVal (VReg r _) x = modifyRegContent r (S.delete x)
clearFromVal _ _ = return ()

addVarToVal :: Val -> SVar -> GenCode ()
addVarToVal (VReg r _) x = modifyRegContent r (S.insert x)
addVarToVal _ _ = return ()

getBestLocation :: SVar -> GenCode Val
getBestLocation x = do
    locs <- getVarLocations x
    let intLocs = S.filter (liftM2 (||) isInt isLabel) locs
    if not (S.null intLocs)
       then return (head (S.toList intLocs))
       else do
            let regLocs = S.filter isReg locs
            if not (S.null regLocs)
            then return (head (S.toList regLocs))
            else return (head (S.toList locs))

saveLiveVars :: S.Set SVar -> GenCode ()
saveLiveVars vars = mapM_ saveVar (S.toList vars)

saveVar :: SVar -> GenCode ()
saveVar x = do
    addr <- getVarAddress x
    locs <- getVarLocations x
    let l = head (S.toList locs)
    when (S.notMember addr locs) (emit (CMov addr l))
    modifyVarLocations x (S.insert addr)

getFreeReg :: GenCode Reg
getFreeReg = do
    rs <- gets regsContent
    let freeRegs = M.keys (M.filter (== S.empty) rs)
    case freeRegs of
      (r:_) -> return r
      [] -> do
            let r = head (M.keys rs)
            freeReg r
            return r

freeReg :: Reg -> GenCode ()
freeReg r = do
    rs <- gets regsContent
    mapM_ saveVar (rs M.! r)
    clearReg r

getSize :: Int -> Size
getSize 1 = Byte
getSize 2 = Word
getSize 4 = DWord
getSize 8 = QWord
getSize _ = DWord

allocNewVar :: SVar -> GenCode ()
allocNewVar x@(SVar _ size) = do
    n <- liftM (+size) $ gets offset
    let v = VMem bp (-n) (getSize size)
    modify (\s -> s { varAddress = M.insert x v (varAddress s)
                    , varCurrentLocations = M.insert x S.empty (varCurrentLocations s)
                    , offset = n })

tryAllocVar :: SVar -> GenCode ()
tryAllocVar x@(SVar var@(VarA n) size) =
    if n > 6 then return ()
             else (do
                 m <- gets varAddress
                 case (m M.!? x) of
                   Just _ -> return ()
                   Nothing -> allocNewVar x)
tryAllocVar x = do
    m <- gets varAddress
    case (m M.!? x) of
      Just _ -> return ()
      Nothing -> allocNewVar x

allocValue :: ValIR -> GenCode ()
allocValue (VarIR var) = tryAllocVar var
allocValue _ = return ()

eax, ecx, edx :: Val
eax = VReg ax DWord
ecx = VReg cx DWord
edx = VReg dx DWord

rbp, rsp :: Val
rbp = VReg bp QWord
rsp = VReg sp QWord

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


allVars :: BBLiveVars -> S.Set SVar
allVars m = S.unions (M.elems (M.map S.unions m))

getV :: SVar -> Var
getV (SVar x _) = x

genCode :: BBGraph -> BBLiveVars -> [Code]
genCode g m = execGenCode (genBBGraph g m)

genBBGraph :: BBGraph -> BBLiveVars -> GenCode ()
genBBGraph g m = do
    let inds = flattenBBGraph g
    let bbs = ids g
    mapM_ tryAllocVar (allVars m)
    n <- gets offset
    let n' = n + stackOffset n
    modify (\s -> s { offset = n' })
    emit (CPush rbp)
    emit (CMov rbp rsp)
    emit (CSub rsp (VInt n' QWord))
    foldM_ declareArgFromStack (16, 6) (Prelude.drop 6 (args g))
    foldM_ declareArgFromArg 1 (Prelude.zip argRegs (Prelude.take 6 (args g)))
    mapM_ (\i -> genBasicBlock (bbs M.! i) (m M.! i)) inds

genBasicBlock :: BasicBlock -> [S.Set SVar] -> GenCode ()
genBasicBlock (BB label xs) liveVars = do
    emit (CLabel (VLabel label))
    let vars = zip (init liveVars) (tail liveVars)
    mapM_ genIRm (zip xs vars)
    mapM_ saveVar (last liveVars)

declareArgFromStack :: (Int, Int) -> Int -> GenCode (Int, Int)
declareArgFromStack (n, i) size = do
    let a = (SVar (VarA n) size)
    let v = VMem bp n (getSize size)
    modify (\s -> s { varAddress = M.insert a v (varAddress s) })
    modifyVarLocations a (S.insert v)
    return (n + size, i + 1)

declareArgFromArg :: Int -> (Reg, Int) -> GenCode Int
declareArgFromArg i (r, size) = do
    let v = VReg r (getSize size)
    let a = SVar (VarA i) size
    modifyVarLocations a (S.insert v)
    modifyRegContent r (S.insert a)
    return (i + 1)

genIRm :: (IR, (S.Set SVar, S.Set SVar)) -> GenCode ()
genIRm (ir, (sBefore, sAfter)) = do
    genIR ir sAfter
    mapM_ clearVar (S.difference sBefore sAfter)

emit_BinOp :: (Val -> Val -> Code) -> SVar -> Val -> Val -> GenCode ()
emit_BinOp constr x v@(VReg r _) v' = do
    emit (constr v v')
    clearReg r
    modifyRegContent r (S.insert x)
    modifyVarLocations x (S.insert v)

genIR_BinOp :: (Val -> Val -> Code) -> SVar -> ValIR -> ValIR -> S.Set SVar -> GenCode ()
genIR_BinOp c x (VarIR y) (IntIR n s) set = do
    let n' = VInt n (getSize s)
    v <- getBestLocation y
    if S.notMember y set && isReg v
       then emit_BinOp c x v n'
       else do
            r <- getFreeReg
            let s' = takeSize v
            let v' = VReg r s'
            emit (CMov v' v)
            emit_BinOp c x v' n'
genIR_BinOp c x (IntIR n s) (VarIR y) set = do
    genIR_BinOp c x (VarIR y) (IntIR n s) set
genIR_BinOp c x (VarIR y1) (VarIR y2) set = do
    v1 <- getBestLocation y1
    v2 <- getBestLocation y2
    if S.notMember y1 set && isReg v1
       then emit_BinOp c x v1 v2
    else if S.notMember y2 set && isReg v2
       then emit_BinOp c x v2 v1
    else do
         r <- getFreeReg
         let s' = takeSize v1
         let v' = VReg r s'
         emit (CMov v' v1)
         emit_BinOp c x v' v2

genIR :: IR -> S.Set SVar -> GenCode ()

-- label
genIR (IR_Label label) _ = emit (CLabel (VLabel label))

-- assignment
genIR (IR_Ass x (LabelIR l)) _ = do
    clearVarLocations x
    setVarLocations x (S.singleton (VLabel l))
genIR (IR_Ass x (IntIR n s)) _ = do
    let size = getSize s
    let v = VInt n size
    clearVarLocations x
    setVarLocations x (S.singleton v)
genIR (IR_Ass x (VarIR y)) _ = do
    locs <- getVarLocations y
    clearVarLocations x
    setVarLocations x locs

-- add
genIR (IR_BinOp (BOpInt IAdd) x v1 v2) set =
    genIR_BinOp CAdd x v1 v2 set

-- mul
genIR (IR_BinOp (BOpInt IMul) x v1 v2) set =
    genIR_BinOp CIMul x v1 v2 set

-- bitand
genIR (IR_BinOp (BOpInt IBitAnd) x v1 v2) set =
    genIR_BinOp CBitAnd x v1 v2 set

-- bitor
genIR (IR_BinOp (BOpInt IBitOr) x v1 v2) set =
    genIR_BinOp CBitOr x v1 v2 set

-- bitxor
genIR (IR_BinOp (BOpInt IBitXor) x v1 v2) set =
    genIR_BinOp CBitXor x v1 v2 set

-- TODO: lshift
-- left shift
genIR (IR_BinOp (BOpInt ILshift) x v1 v2) set =
    genIR_BinOp CShiftL x v1 v2 set

-- TODO: rshift
-- right shift
genIR (IR_BinOp (BOpInt IRshift) x v1 v2) set =
    genIR_BinOp CShiftR x v1 v2 set

-- memory read
-- genIR (IR_MemRead x (VarIR y)) set = do


-- jump
genIR (IR_Jump label) set = do
    mapM_ saveVar set
    emit (CJump (VLabel label))

-- conditional jump
genIR (IR_CondJump (VarIR x) op (IntIR n s) label) set = do
    mapM_ saveVar set
    v <- getBestLocation x
    let n' = VInt n (getSize s)
    emit (CCmp v n')
    emit (CCondJump op (VLabel label))
genIR (IR_CondJump (IntIR n s) op (VarIR x) label) set = do
    mapM_ saveVar set
    v <- getBestLocation x
    let n' = VInt n (getSize s)
    emit (CCmp v n')
    emit (CCondJump op (VLabel label))
genIR (IR_CondJump (VarIR x) op (VarIR y) label) set = do
    mapM_ saveVar set
    v1 <- getBestLocation x
    v2 <- getBestLocation y
    if isReg v1 || isReg v2
       then emit (CCmp v1 v2)
       else do
            r <- getFreeReg
            let r' = VReg r (takeSize v1)
            emit (CMov r' v1)
            emit (CCmp r' v2)
    emit (CCondJump op (VLabel label))



-- return
genIR (IR_Return (IntIR n s)) _ = do
    let v = VReg ax (getSize s)
    emit (CMov v (VInt n (getSize s)))
    emit CLeave
    emit CRet
genIR (IR_Return (VarIR x)) _ = do
    let v = VReg ax ((\(SVar _ s) -> getSize s) x)
    vals <- liftM (M.! ax) $ gets regsContent
    when (S.notMember x vals)
         (do
          v' <- getBestLocation x
          emit (CMov v v'))
    emit CLeave
    emit CRet
genIR IR_VoidReturn _ = do
    emit (CLeave)
    emit (CRet)

-- nop
genIR (IR_Nop) _ = emit  CNop

-- undefined
genIR _ _ = emit CNop

