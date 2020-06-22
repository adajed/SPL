module GenerateIR where

import AbsSPL
import ClassInfo
import ErrM
import GenIR
import IR
import Utils
import Token
import Type

import FreeVariables ( getFreeVars, substitute )

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Control.Monad ( liftM, foldM, when, unless, void )
import Control.Monad.Trans ( lift )

import Control.Monad.Trans.State

binop :: BinOp -> ValIR -> ValIR -> Int -> GenIR ValIR
binop op v1 v2 size = liftM VarIR $ emitIR_ToTemp size (\t -> IR_BinOp op t v1 v2)

add, mul, sub :: ValIR -> ValIR -> Int -> GenIR ValIR
add = binop IAdd
mul = binop IMul
sub = binop ISub

call :: ValIR -> [ValIR] -> Int -> GenIR ValIR
call f xs size = liftM VarIR $ emitIR_ToTemp size (\t -> IR_Call t f xs)

memread :: Int -> ValIR -> GenIR ValIR
memread size x  = liftM VarIR $ emitIR_ToTemp size (\t -> IR_MemRead t x)

memsave :: ValIR -> ValIR -> Int -> GenIR ()
memsave x y size = emitIR (IR_MemSave x y size)


getField :: ValIR -> Int -> Int -> GenIR ValIR
getField x offset size =
    memread size =<< add x (IntIR offset 8) 8

setField :: ValIR -> ValIR -> Int -> Int -> GenIR ()
setField x v offset size = do
    ptr <- add x (IntIR offset 8) 8
    emitIR (IR_MemSave ptr v size)

addLambda :: Type T -> VIdent -> [Argument T] -> Stmt T -> GenIR ()
addLambda t name args stmt = modify (\s -> s { lambdas = (t, name, args, stmt):(lambdas s) })

defaultValue :: Type a -> ValIR
defaultValue (Int _) = IntIR 0 4
defaultValue (Bool _) = IntIR 0 1
defaultValue (Void _) = VoidIR
defaultValue (Class _ _) = IntIR 0 8
defaultValue (Array _ _) = IntIR 0 8
defaultValue (Fun _ _ _) = IntIR 0 8
defaultValue _ = IntIR 0 4

runGenerateIR :: Program T -> Err ProgramIR
runGenerateIR program = runGenIR (generateIR_Program program)

generateIR_Program :: Program T -> GenIR ()
generateIR_Program (Prog t topdefs) = do
    buildClassGraph topdefs
    mapM_ declare_TopDef topdefs
    mapM_ generateIR_TopDef topdefs
    generateIR_lambda
    classNames <- liftM M.keys $ gets classInfo
    mapM_ generateIR_Destructor classNames
    mapM_ generateIR_Constructor classNames
    mapM_ generateIR_VTable classNames

generateIR_VTable :: CIdent -> GenIR ()
generateIR_VTable cls = do
    info <- liftM (!!! cls) $ gets classInfo
    let f (CMethod offset name) = (offset, name)
    let methods = getAllMethods info
    let entries' = map (\m -> f ((attrs info) !!! m)) methods
    let entries = L.sortBy (\x y -> compare (fst x) (fst y)) entries'
    let lName = vtableName cls
    let ir = map (\(_, name) -> IR_DataQ (LabelIR name)) entries
    modify (\s -> s { output = (output s) { dataSection = M.insert lName ir (dataSection (output s)) } })


declare_TopDef :: TopDef T -> GenIR ()
declare_TopDef (FnDef _ ty name args _) = declareFunction name (toUnit ty) args
declare_TopDef (ClassDef _ cls extends args) = declareClass cls extends args

generateIR_lambda :: GenIR ()
generateIR_lambda = do
    ls <- gets lambdas
    case ls of
      [] -> return ()
      ((t, name, args, stmt):ls) -> do
                                modify (\s -> s { lambdas = tail (lambdas s) })
                                generateIR_TopDef (FnDef (Void ()) t name args (Bl (Void ()) [stmt]))
                                generateIR_lambda

generateIR_TopDef :: TopDef T -> GenIR ()
generateIR_TopDef (FnDef _ t name args (Bl _ stmts)) = do
    modify (\s -> s { currentOutput = [] } )
    emitIR (IR_Label name)
    pushEmptyEnv
    mapM_ declareArg args
    let bl = Bl voidT (if t == (Void (Void ())) then stmts ++ [VRet voidT] else stmts)
    generateIR_Stmt (BStmt (Void ()) bl)
    void popEnv
    saveCurrentOutput name
generateIR_TopDef (ClassDef _ cls _ args) =
    mapM_ (generateIR_ClassElem cls) args

generateIR_ClassElem :: CIdent -> ClassElem T -> GenIR ()
generateIR_ClassElem cls (Field _ _ _) = return ()
generateIR_ClassElem cls (Method _ t name args block) =
    let fName = methodName cls name
        selfArg = Arg voidT (Class voidT cls) self
     in generateIR_TopDef (FnDef voidT t fName (selfArg:args) block)
generateIR_ClassElem cls (Constr _ args block) = do
    let t = map (\(Arg _ t' _) -> toUnit t') args
    let tCls = toVoid (classT cls)
    let tV = toVoid voidT
    name <- liftM ((M.! t) . constructors . (M.! cls)) $ gets classInfo
    modify (\s -> s { currentOutput = [] } )
    emitIR (IR_Label name)
    pushEmptyEnv
    mapM_ declareArg args
    let alloc = LabelIR (allocatorName cls)
    obj <- emitIR_ToTemp 8 (\t -> IR_Call t alloc [])
    memsave (VarIR obj) (IntIR 1 4) 4
    modifyTopEnv (M.insert self (obj, toUnit tCls))
    generateIR_Stmt (BStmt voidT block)
    void popEnv
    saveCurrentOutput name

declareArg :: Argument T -> GenIR ()
declareArg (Arg _ t name) = do
    var <- declareVar (fmap (const ()) t) name
    emitIR (IR_Argument var)

isFreeable :: T -> Bool
isFreeable (Int ()) = False
isFreeable (Bool ()) = False
isFreeable _ = True

decreaseRefCount :: ValIR -> GenIR ValIR
decreaseRefCount x = do
    cnt <- memread 4 x
    newcnt <- sub cnt (IntIR 1 4) 4
    emitIR (IR_MemSave x newcnt 4)
    return newcnt

releaseObject :: T -> ValIR -> GenIR ()
releaseObject (Int ()) _ = return ()
releaseObject (Bool ()) _ = return ()
releaseObject (Null ()) _ = return ()
releaseObject t@(Class _ cls) x =
    if isLambdaEnv cls
       then return ()
       else do
               l <- getFreshLabel
               emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
               f <- getField x 12 8
               emitIR (IR_VoidCall f [x])
               emitIR (IR_Label l)
releaseObject t@(Array _ ty) x = do
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
    refcnt <- decreaseRefCount x
    emitIR (IR_CondJump refcnt IR.NEQ (IntIR 0 4) l)
    when (isFreeable ty) (do
            arr <- add x (IntIR 8 8) 8
            size <- memread 4 =<< add x (IntIR 4 8) 8
            i <- emitIR_ToTemp 4 (\t -> IR_Ass t (IntIR 0 4))
            l1 <- getFreshLabel
            l2 <- getFreshLabel
            emitIR (IR_Jump l2)
            emitIR (IR_Label l1)
            pos <- mul (VarIR i) (IntIR (sizeOf ty) 4) 4
            y' <- add arr pos 8
            y <- memread (sizeOf ty) =<< add arr pos 8
            releaseObject ty y
            i' <- add (VarIR i) (IntIR 1 4) 4
            emitIR (IR_Ass i i')
            emitIR (IR_Label l2)
            emitIR (IR_CondJump (VarIR i) IR.LTH size l1)
                         )
    emitIR (IR_VoidCall (LabelIR (VIdent "freeMemory")) [x])
    emitIR (IR_Label l)
releaseObject t@(Fun _ _ _) x = do
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
    refcnt <- decreaseRefCount x
    emitIR (IR_CondJump refcnt IR.NEQ (IntIR 0 4) l)
    f <- memread 8 =<< add x (IntIR 20 8) 8
    c <- memread 8 =<< add x (IntIR 12 8) 8
    emitIR (IR_VoidCall f [c])
    emitIR (IR_VoidCall (LabelIR (VIdent "freeMemory")) [x])
    emitIR (IR_Label l)

acquireObject :: T -> ValIR -> GenIR ()
acquireObject (Int ()) _ = return ()
acquireObject (Bool ()) _ = return ()
acquireObject (Null ()) _ = return ()
acquireObject t x = do
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
    cnt <- memread 4 x
    newcnt <- add cnt (IntIR 1 4) 4
    emitIR (IR_MemSave x newcnt 4)
    emitIR (IR_Label l)

decrementRefCount :: T -> ValIR -> GenIR ()
decrementRefCount (Int ()) _ = return ()
decrementRefCount (Bool ()) _ = return ()
decrementRefCount (Null ()) _ = return ()
decrementRefCount t x = do
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
    decreaseRefCount x
    emitIR (IR_Label l)


generateIR_Destructor :: CIdent -> GenIR ()
generateIR_Destructor cls = do
    let fName = destructorName cls
    modify (\s -> s { currentOutput = [] })
    emitIR (IR_Label fName)
    pushEmptyEnv
    let varName = VIdent "obj"
    var <- declareVar (classT cls) varName
    let x = VarIR var
    emitIR (IR_Argument var)
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.NEQ (IntIR 0 8) l)
    emitIR IR_VoidReturn
    emitIR (IR_Label l)
    unless (isLambdaEnv cls)
        (do
         cnt <- memread 4 x
         newcnt <- sub cnt (IntIR 1 4) 4
         emitIR (IR_MemSave x newcnt 4)
         l <- getFreshLabel
         emitIR (IR_CondJump newcnt IR.EQU (IntIR 0 4) l)
         emitIR IR_VoidReturn
         emitIR (IR_Label l)
        )
    info <- liftM (!!! cls) $ gets classInfo
    mapM_ (freeField info x) (getAllFields info)
    let free = LabelIR (VIdent "freeMemory")
    emitIR (IR_VoidCall free [x])
    emitIR IR_VoidReturn
    void popEnv
    saveCurrentOutput fName

freeField :: ClassInfo -> ValIR -> VIdent -> GenIR ()
freeField info obj name = do
    let (CField offset _) = (attrs info) !!! name
    let t = (fieldType info) !!! name
    x <- getField obj offset (sizeOf t)
    releaseObject t x


generateIR_Constructor :: CIdent -> GenIR ()
generateIR_Constructor cls = do
    let fName = allocatorName cls
    modify (\s -> s { currentOutput = [] })
    emitIR (IR_Label fName)
    pushEmptyEnv
    size <- liftM (classSize . (!!! cls)) $ gets classInfo
    v <- generateIR_Expr_Alloc (EInt (Int ()) (toInteger size))
    setField v (IntIR 0 4) 0 4
    unless (isLambdaEnv cls)
        (do
            setField v (LabelIR (vtableName cls)) 4 8
            setField v (LabelIR (destructorName cls)) 12 8
        )
    info <- liftM (!!! cls) $ gets classInfo
    mapM_ (initField info v) (getAllFields info)
    emitIR (IR_Return v)
    void popEnv
    saveCurrentOutput fName

initField :: ClassInfo -> ValIR -> VIdent -> GenIR ()
initField info obj name = do
    let (CField offset _) = (attrs info) !!! name
    let t = (fieldType info) !!! name
    let v = defaultValue t
    setField obj v offset (sizeOf t)

releaseWholeStack :: GenIR ()
releaseWholeStack = do
    envs <- gets varenv
    let m env = mapM_ (\(x, t) -> releaseObject t (VarIR x)) (M.elems env)
    mapM_ m envs

generateIR_Stmt :: Stmt T -> GenIR ()
generateIR_Stmt (Empty _) = return ()
generateIR_Stmt (BStmt _ (Bl _ stmts)) = do
    pushEmptyEnv
    mapM_ generateIR_Stmt stmts
    env <- popEnv
    mapM_ (\(x, t) -> releaseObject t (VarIR x)) (M.elems env)
generateIR_Stmt (Decl _ t items) = do
    mapM_ (generateIR_Decl (fmap (const ()) t)) items
generateIR_Stmt (Ass t expr1 expr2) = do
    value <- generateIR_Expr expr2
    acquireObject (exprType expr2) value
    releaseObject (exprType expr1) =<< generateIR_Expr expr1
    fAss <- generateIR_LExpr expr1
    fAss value
generateIR_Stmt (Incr _ expr) = do
    value <- generateIR_Expr expr
    f <- generateIR_LExpr expr
    f =<< add value (IntIR 1 4) 4
generateIR_Stmt (Decr _ expr) = do
    value <- generateIR_Expr expr
    f <- generateIR_LExpr expr
    f =<< sub value (IntIR 1 4) 4
generateIR_Stmt (Ret _ expr) = do
    value <- generateIR_Expr expr
    acquireObject (exprType expr) value
    releaseWholeStack
    decrementRefCount (exprType expr) value
    emitIR (IR_Return value)
generateIR_Stmt (VRet _) = do
    releaseWholeStack
    emitIR IR_VoidReturn
generateIR_Stmt (Cond _ expr stmt) = do
    lTrue <- getFreshLabel
    lFalse <- getFreshLabel
    generateIR_JumpExpr expr lTrue lFalse
    emitIR (IR_Label lTrue)
    generateIR_Stmt stmt
    emitIR (IR_Label lFalse)
generateIR_Stmt (CondElse _ expr stmt1 stmt2) = do
    lTrue <- getFreshLabel
    lFalse <- getFreshLabel
    lEnd <- getFreshLabel
    generateIR_JumpExpr expr lTrue lFalse
    emitIR (IR_Label lTrue)
    generateIR_Stmt stmt1
    emitIR (IR_Jump lEnd)
    emitIR (IR_Label lFalse)
    generateIR_Stmt stmt2
    emitIR (IR_Label lEnd)
generateIR_Stmt (While _ expr stmt) = do
    lTrue <- getFreshLabel
    lFalse <- getFreshLabel
    lCheck <- getFreshLabel
    emitIR (IR_Jump lCheck)
    emitIR (IR_Label lTrue)
    generateIR_Stmt stmt
    emitIR (IR_Label lCheck)
    generateIR_JumpExpr expr lTrue lFalse
    emitIR (IR_Label lFalse)
generateIR_Stmt (ForUp _ name exprS exprE exprI stmt) = do
    lLoop <- getFreshLabel
    lEnd <- getFreshLabel
    valS <- generateIR_Expr exprS
    valE <- generateIR_Expr exprE
    valI <- generateIR_Expr exprI
    i <- emitIR_ToTemp 4 (\t -> IR_Ass t valS)
    emitIR (IR_Jump lEnd)
    emitIR (IR_Label lLoop)
    let env = M.fromList [(name, (i, Int ()))]
    modify (\s -> s { varenv = env:(varenv s) })
    generateIR_Stmt stmt
    temp <- add (VarIR i) valI 4
    emitIR (IR_Ass i temp)
    void popEnv
    emitIR (IR_Label lEnd)
    emitIR (IR_CondJump (VarIR i) IR.LTH valE lLoop)
generateIR_Stmt (ForDown _ name exprS exprE exprI stmt) = do
    lLoop <- getFreshLabel
    lEnd <- getFreshLabel
    valS <- generateIR_Expr exprS
    valE <- generateIR_Expr exprE
    valI <- generateIR_Expr exprI
    i <- emitIR_ToTemp 4 (\t -> IR_Ass t valS)
    emitIR (IR_Jump lEnd)
    emitIR (IR_Label lLoop)
    let env = M.fromList [(name, (i, Int ()))]
    modify (\s -> s { varenv = env:(varenv s) })
    generateIR_Stmt stmt
    temp <- sub (VarIR i) valI 4
    emitIR (IR_Ass i temp)
    void popEnv
    emitIR (IR_Label lEnd)
    emitIR (IR_CondJump (VarIR i) IR.GTH valE lLoop)
generateIR_Stmt (ForEach _ name expr stmt) = do
    lLoop <- getFreshLabel
    lEnd <- getFreshLabel
    let (Array _ ty) = exprType expr
    arr <- generateIR_Expr expr
    size <- getField arr 4 4
    arrStart <- add arr (IntIR 8 8) 8
    i <- emitIR_ToTemp 4 (\t -> IR_Ass t (IntIR 0 4))
    emitIR (IR_Jump lEnd)
    emitIR (IR_Label lLoop)
    temp <- mul (VarIR i) (IntIR (sizeOf ty) 4) 4
    elemPtr <- add arrStart temp 8
    elem <- emitIR_ToTemp (sizeOf ty) (\t -> IR_MemRead t elemPtr)
    let env = M.fromList [(name, (elem, ty))]
    modify (\s -> s { varenv = env:(varenv s) })
    generateIR_Stmt stmt
    temp <- add (VarIR i) (IntIR 1 4) 4
    emitIR (IR_Ass i temp)
    void popEnv
    emitIR (IR_Label lEnd)
    emitIR (IR_CondJump (VarIR i) IR.LTH size lLoop)
generateIR_Stmt (SExp _ expr) = do
    value <- generateIR_Expr expr
    return ()

generateIR_Decl :: T -> Item T -> GenIR ()
generateIR_Decl t (NoInit _ name) = do
    var <- declareVar t name
    emitIR (IR_Ass var (defaultValue t))
generateIR_Decl t (Init _ name expr) = do
    var <- declareVar t name
    temp <- generateIR_Expr expr
    acquireObject (exprType expr) temp
    emitIR (IR_Ass var temp)


relop :: AbsSPL.RelOp a -> IR.RelOp
relop op = case op of
              AbsSPL.LTH _ -> IR.LTH
              AbsSPL.LE _  -> IR.LEQ
              AbsSPL.GTH _ -> IR.GTH
              AbsSPL.GE _  -> IR.GEQ
              AbsSPL.EQU _ -> IR.EQU
              AbsSPL.NE _  -> IR.NEQ


generateIR_Expr :: Expr T -> GenIR ValIR
generateIR_Expr (ETypedExpr _ _ expr) = generateIR_Expr expr
generateIR_Expr (EInt _ n) = return (IntIR (fromInteger n) 4)
generateIR_Expr (ETrue _) = return (IntIR 1 1)
generateIR_Expr (EFalse _) = return (IntIR 0 1)
generateIR_Expr (ENull _) = return (IntIR 0 8)
generateIR_Expr (EVar _ name) = liftM VarIR $ getVar name
generateIR_Expr (EField _ expr field) =
    case exprType expr of
      Class _ cls -> do
            attr <- liftM ((!!! field) . attrs . (!!! cls)) $ gets classInfo
            case attr of
              CField offset size -> do
                                    x <- generateIR_Expr expr
                                    getField x offset size
      Array _ _ -> do -- accessing length of array
            x <- generateIR_Expr expr
            getField x 4 4
      _ -> fail "error"
generateIR_Expr expr@(EArrAcc type_ arr index) = do
    arrV <- generateIR_Expr arr
    ind <- generateIR_Expr index
    let size = sizeOf type_
    arrV' <- add arrV (IntIR 8 8) 8
    v' <- mul ind (IntIR size 4) 4
    memread size =<< add arrV' v' 8
generateIR_Expr (EApp ty e@(EField _ expr field) args) = do
    obj <- generateIR_Expr expr
    case exprType expr of
      Class _ cls -> do
            attr <- liftM ((!!! field) . attrs . (!!! cls)) $ gets classInfo
            case attr of
              CField offset size -> do
                  xs <- mapM generateIR_Expr args
                  mapM_ (uncurry acquireObject) (zip (map exprType args) xs)
                  f <- generateIR_AppExpr e ty
                  f xs
              CMethod offset _ -> do
                  vtable <- getField obj 4 8
                  method <- getField vtable offset 8
                  xs <- mapM generateIR_Expr args
                  acquireObject (Class () cls) obj
                  mapM_ (uncurry acquireObject) (zip (map exprType args) xs)
                  call method (obj:xs) (sizeOf ty)
generateIR_Expr (EApp ty fExpr args) = do
    xs <- mapM generateIR_Expr args
    mapM_ (uncurry acquireObject) (zip (map exprType args) xs)
    f <- generateIR_AppExpr fExpr ty
    f xs
generateIR_Expr (EUnaryOp _ op expr) = generateIR_UnOp op' expr
    where op' = case op of
                  Neg _    -> INeg
                  Not _    -> BNot
                  BitNot _ -> IBitNot
generateIR_Expr (EMul _ expr1 op expr2) = generateIR_BinOp op' expr1 expr2
    where op' = case op of
                  Times _  -> IMul
                  Div _    -> IDiv
                  Mod _    -> IMod
                  LShift _ -> ILshift
                  RShift _ -> IRshift
                  BitAnd _ -> IBitAnd
                  BitOr _  -> IBitOr
                  BitXor _ -> IBitXor
generateIR_Expr (EAdd _ expr1 op expr2) = generateIR_BinOp op' expr1 expr2
    where op' = case op of
                  Plus _  -> IAdd
                  Minus _ -> ISub
generateIR_Expr e@(ERel _ _ _ _) = generateIR_BoolExpr e
generateIR_Expr e@(EAnd _ _ _) = generateIR_BoolExpr e
generateIR_Expr e@(EOr _ _ _) = generateIR_BoolExpr e
generateIR_Expr (EObjNew t cls exprs) = do
    let t = map exprType exprs
    constrs <- liftM (constructors . (M.! cls)) $ gets classInfo
    g <- gets classGraph
    let name = snd $ head $ filter ((\t' -> isSubtypeList g t' t) . fst) (M.toList constrs)
    xs <- mapM generateIR_Expr exprs
    mapM_ (uncurry acquireObject) (zip (map exprType exprs) xs)
    call (LabelIR name) xs 8
generateIR_Expr (EArrNew tt t expr) = do
    let n = sizeOf t
    v1 <- generateIR_Expr expr
    v2 <- mul v1 (IntIR n 4) 4
    v3 <- add v2 (IntIR 8 4) 4
    let f = LabelIR (VIdent "allocMemory")
    ptr <- call f [v3] 8
    emitIR (IR_MemSave ptr (IntIR 0 4) 4)
    setField ptr v1 4 4
    let initVal = defaultValue t
    i <- emitIR_ToTemp 4 (\t -> IR_Ass t (IntIR 0 4))
    p' <- add ptr (IntIR 8 8) 8
    lLoop <- getFreshLabel
    lEnd <- getFreshLabel
    emitIR (IR_Jump lEnd)
    emitIR (IR_Label lLoop)
    p1 <- mul (VarIR i) (IntIR n 4) 4
    p2 <- add p' p1 8
    emitIR (IR_MemSave p2 initVal n)
    i' <- add (VarIR i) (IntIR 1 4) 4
    emitIR (IR_Ass i i')
    emitIR (IR_Label lEnd)
    emitIR (IR_CondJump (VarIR i) IR.LTH v1 lLoop)
    return ptr
generateIR_Expr e@(ELambda ty args stmt) = do
    lambdaN <- gets lambdaCounter
    let envCls = CIdent ("env__class__" ++ show lambdaN)
    let env = VIdent ("__env__" ++ show lambdaN)
    let lambdaName = VIdent ("__lambda__" ++ show lambdaN)
    let freeVars = S.toList (getFreeVars (Ret (Void ()) e))
    let allocName = allocatorName envCls
    let initInfo = ClassInfo { attrs = M.empty
                             , fieldType = M.empty
                             , classSize = 4
                             , vtableSize = 0
                             , constructors = M.singleton [] allocName
                             }
    info <- foldM buildOffsets initInfo freeVars
    modify (\s -> s { lambdaCounter = lambdaN + 1 })
    modify (\s -> s { classInfo = M.insert envCls info (classInfo s) })
    v <- generateIR_Expr_Alloc (EInt (Int ()) 28)
    emitIR (IR_MemSave v (IntIR 0 4) 4)
    lPtr <- add v (IntIR 4 8) 8
    emitIR (IR_MemSave lPtr (LabelIR lambdaName) 8)
    v1 <- add v (IntIR 12 8) 8
    v2 <- call (LabelIR allocName) [] 8
    mapM_ (setupEnv info v2) freeVars
    emitIR (IR_MemSave v1 v2 8)
    lPtr <- add v (IntIR 20 8) 8
    let destrName = destructorName envCls
    emitIR (IR_MemSave lPtr (LabelIR destrName) 8)
    let envType = Class (Void ()) envCls
    let stmt' = substitute envType env (S.fromList freeVars) stmt
    let tOut = case ty of { Fun _ t _ -> fmap (const (Void ())) t; _ -> Void (Void ()) }
    let envArg = Arg (Void ()) envType env
    addLambda tOut lambdaName (args ++ [envArg]) stmt'
    return v

setupEnv :: ClassInfo -> ValIR -> VIdent -> GenIR ()
setupEnv info v x = do
    var <- liftM VarIR $ getVar x
    let (CField offset size) = (attrs info) !!! x
    let ty = (fieldType info) !!! x
    t <- add v (IntIR offset 8) size
    acquireObject ty var
    emitIR (IR_MemSave t var size)

generateIR_Expr_Alloc :: Expr T -> GenIR ValIR
generateIR_Expr_Alloc expr = do
    let fname = LabelIR (VIdent "allocMemory")
    v <- generateIR_Expr expr
    call fname [v] 8

buildOffsets :: ClassInfo -> VIdent -> GenIR ClassInfo
buildOffsets info x = do
    t <- getVarType x
    let size = sizeOf t
    let attr = CField (classSize info) size
    return (info { attrs = M.insert x attr (attrs info)
                 , fieldType = M.insert x t (fieldType info)
                 , classSize = (classSize info) + size
                 })


generateIR_UnOp :: UnOp -> Expr T -> GenIR ValIR
generateIR_UnOp op expr = do
    v <- generateIR_Expr expr
    let size = sizeOf (exprType expr)
    liftM VarIR $ emitIR_ToTemp size (\t -> IR_UnOp op t v)

generateIR_BinOp :: BinOp -> Expr T -> Expr T -> GenIR ValIR
generateIR_BinOp op expr1 expr2 = do
    v1 <- generateIR_Expr expr1
    v2 <- generateIR_Expr expr2
    let size = sizeOf (exprType expr1)
    liftM VarIR $ emitIR_ToTemp size (\t -> IR_BinOp op t v1 v2)

generateIR_LExpr :: Expr T -> GenIR (ValIR -> GenIR ())
generateIR_LExpr (EVar _ name) = do
    var <- getVar name
    return (\v -> emitIR (IR_Ass var v))
generateIR_LExpr (EArrAcc type_ arr expr) = do
    x <- generateIR_Expr arr
    ind <- generateIR_Expr expr
    let size = sizeOf type_
    x' <- add x (IntIR 8 8) 8
    v' <- mul ind (IntIR size 4) 4
    loc <- add x' v' 8
    return (\v -> emitIR (IR_MemSave loc v size))
generateIR_LExpr (EField _ expr field) =
    case exprType expr of
      Class _ cls -> do
            attr <- liftM ((!!! field) . attrs . (!!! cls)) $ gets classInfo
            case attr of
              CField offset size -> do
                                    x <- generateIR_Expr expr
                                    y <- add x (IntIR offset 8) 8
                                    return (\v -> emitIR (IR_MemSave y v size))
      _ -> fail "error"

generateIR_AppExpr :: Expr T -> T -> GenIR ([ValIR] -> GenIR ValIR)
generateIR_AppExpr (EVar _ name) ty = do
    v <- getMaybeVar name
    case v of
      Just var -> return (\xs -> do
                                 let v = VarIR var
                                 f <- memread 8 =<< add v (IntIR 4 8) 8
                                 envPtr <- memread 8 =<< add v (IntIR 12 8) 8
                                 call f (xs ++ [envPtr]) (sizeOf ty)
                         )
      Nothing -> return (\xs -> call (LabelIR name) xs (sizeOf ty))
generateIR_AppExpr fExpr ty = do
    v <- generateIR_Expr fExpr
    return (\xs -> do
                   f <- memread 8 =<< add v (IntIR 4 8) 8
                   envPtr <- memread 8 =<< add v (IntIR 12 8) 8
                   call f (xs ++ [envPtr]) (sizeOf ty)
           )

generateIR_JumpExpr :: Expr T -> VIdent -> VIdent -> GenIR ()
generateIR_JumpExpr (ERel _ e1 op e2) lTrue lFalse = do
    v1 <- generateIR_Expr e1
    v2 <- generateIR_Expr e2
    emitIR (IR_CondJump v1 (relop op) v2 lTrue)
    emitIR (IR_Jump lFalse)
generateIR_JumpExpr (EAnd _ e1 e2) lTrue lFalse = do
    lFresh <- getFreshLabel
    generateIR_JumpExpr e1 lFresh lFalse
    emitIR (IR_Label lFresh)
    generateIR_JumpExpr e2 lTrue lFalse
generateIR_JumpExpr (EOr _ e1 e2) lTrue lFalse = do
    lFresh <- getFreshLabel
    generateIR_JumpExpr e1 lTrue lFresh
    emitIR (IR_Label lFresh)
    generateIR_JumpExpr e2 lTrue lFalse
generateIR_JumpExpr expr lTrue lFalse = generateIR_JumpExpr expr' lTrue lFalse
    where expr' = ERel (Bool ()) expr (AbsSPL.EQU (Void ())) (ETrue (Bool ()))

generateIR_BoolExpr :: Expr T -> GenIR ValIR
generateIR_BoolExpr expr = do
    lTrue <- getFreshLabel
    lFalse <- getFreshLabel
    lEnd <- getFreshLabel
    t <- getFreshTemp
    let x = SVar t 1
    generateIR_JumpExpr expr lTrue lFalse
    emitIR (IR_Label lTrue)
    emitIR (IR_Ass x (IntIR 1 1))
    emitIR (IR_Jump lEnd)
    emitIR (IR_Label lFalse)
    emitIR (IR_Ass x (IntIR 0 1))
    emitIR (IR_Label lEnd)
    return (VarIR x)

exprType :: Expr T -> T
exprType (ENull t) = t
exprType (EInt t _) = t
exprType (ETrue t) = t
exprType (EFalse t) = t
exprType (EVar t _) = t
exprType (EField t _ _) = t
exprType (EArrAcc t _ _) = t
exprType (EApp t _ _) = t
exprType (EUnaryOp t _ _) = t
exprType (EMul t _ _ _) = t
exprType (EAdd t _ _ _) = t
exprType (ERel t _ _ _) = t
exprType (EAnd t _ _) = t
exprType (EOr t _ _) = t
exprType (EObjNew t _ _) = t
exprType (EArrNew t _ _) = t
exprType (ELambda t _ _) = t

