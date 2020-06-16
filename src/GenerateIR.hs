module GenerateIR where

import AbsSPL
import ErrM
import IR

import FreeVariables ( getFreeVars, substitute )

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Control.Monad ( liftM, foldM, when, unless, void )
import Control.Monad.Trans ( lift )

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

type Offset = Int

type T = Type ()

data ClassInfo = ClassInfo { classSize   :: Int
                           , fieldOffset :: M.Map VIdent Offset
                           , fieldSize   :: M.Map VIdent Int
                           , fieldType   :: M.Map VIdent T
                           }

data SState = SState { tempCounter :: Int
                     , levelCounter :: Int
                     , labelCounter :: Int
                     , classInfo :: M.Map CIdent ClassInfo
                     , varenv :: [M.Map VIdent (SVar, T)]
                     , output :: M.Map VIdent [IR]
                     , currentOutput :: [IR]
                     , lambdas :: [(Type T, VIdent, [Argument T], Stmt T)]
                     , lambdaCounter :: Int
                     }

type GenerateIR a = StateT SState Err a

sizeOf :: Type a -> Int
sizeOf (Int _) = 4
sizeOf (Bool _) = 1
sizeOf (Void _) = 0
sizeOf (Array _ _) = 8
sizeOf (Fun _ _ _) = 8
sizeOf (Class _ _) = 8
sizeOf (Null _) = 8


initialSState :: SState
initialSState = SState { tempCounter  = 0
                       , levelCounter = 0
                       , labelCounter = 0
                       , classInfo = M.empty
                       , varenv = []
                       , output = M.empty
                       , currentOutput = []
                       , lambdas = []
                       , lambdaCounter = 0
                       }

runGenerateIR :: Program T -> Err (M.Map VIdent [IR])
runGenerateIR program = liftM output $ execStateT m initialSState
    where m = generateIR_Program program

getsSState :: (SState -> a) -> GenerateIR a
getsSState f = gets f

modifySState :: (SState -> SState) -> GenerateIR ()
modifySState f = modify f

emitIR :: IR -> GenerateIR ()
emitIR ir = modifySState (\s -> s { currentOutput = ir:(currentOutput s) } )

emitIR_ToTemp :: Int -> (SVar -> IR) -> GenerateIR SVar
emitIR_ToTemp size f = do
    t <- getFreshTemp
    let v = SVar t size
    emitIR (f v)
    return v

getFreshTemp :: GenerateIR Var
getFreshTemp = do
    i <- getsSState tempCounter
    modifySState (\s -> s { tempCounter = i + 1 })
    return (VarT i)

declareVar :: T -> VIdent -> GenerateIR SVar
declareVar t name = do
    l <- gets levelCounter
    let name' = VIdent (show name ++ "_" ++ show l)
    let v = SVar (VarN name') (sizeOf t)
    env <- liftM ((M.insert name (v, t)) . head) $ gets varenv
    modifySState (\s -> s { varenv = env:(tail (varenv s)) } )
    return v

declareTopDef :: TopDef T -> GenerateIR ()
declareTopDef (FnDef _ _ name _ _) = return ()
declareTopDef (ClDef _ name args) =
    let info = generateOffsets args
     in modify (\s -> s { classInfo = M.insert name info (classInfo s) })

generateOffsets :: [ClassArgument T] -> ClassInfo
generateOffsets args = foldl h initInfo args
    where initInfo = ClassInfo { classSize = 4, fieldType = M.empty, fieldOffset = M.empty, fieldSize = M.empty }
          h :: ClassInfo -> ClassArgument T -> ClassInfo
          h acc (Field _ t xs) = foldl (h' (sizeOf t) (fmap (const ()) t)) acc xs
          h' :: Int -> T -> ClassInfo -> VIdent -> ClassInfo
          h' size t acc name = ClassInfo { classSize = (classSize acc) + size
                                         , fieldType = M.insert name t (fieldType acc)
                                         , fieldOffset = M.insert name (classSize acc) (fieldOffset acc)
                                         , fieldSize = M.insert name size (fieldSize acc)
                                         }


getMaybeVar :: VIdent -> GenerateIR (Maybe SVar)
getMaybeVar name = do
    let f [] = return Nothing
        f (e:es) = if M.member name e
                      then return (Just (fst (e M.! name)))
                      else f es
    f =<< gets varenv

getVar :: VIdent -> GenerateIR SVar
getVar name = do
    x <- getMaybeVar name
    case x of
      Just var -> return var
      Nothing -> errorVariableNotDeclared name

getVarType :: VIdent -> GenerateIR T
getVarType name = do
    let f [] = errorVariableNotDeclared name
        f (e:es) = if M.member name e
                      then return (snd (e M.! name))
                      else f es
    f =<< gets varenv

getFreshLabel :: GenerateIR VIdent
getFreshLabel = do
    i <- getsSState labelCounter
    modifySState (\s -> s { labelCounter = i + 1 } )
    return $ VIdent $ ".L" ++ show i

addLambda :: Type T -> VIdent -> [Argument T] -> Stmt T -> GenerateIR ()
addLambda t name args stmt = modify (\s -> s { lambdas = (t, name, args, stmt):(lambdas s) })

defaultValue :: Type a -> ValIR
defaultValue (Int _) = IntIR 0 4
defaultValue (Bool _) = IntIR 0 1
defaultValue (Void _) = VoidIR
defaultValue (Class _ _) = IntIR 0 8
defaultValue (Array _ _) = IntIR 0 8
defaultValue (Fun _ _ _) = IntIR 0 8
defaultValue _ = IntIR 0 4

errorVariableNotDeclared :: VIdent -> GenerateIR a
errorVariableNotDeclared name =
    fail ("Variable " ++ show name ++ " not declared")

pushEmptyEnv :: GenerateIR ()
pushEmptyEnv = modify (\s -> s { levelCounter = (levelCounter s) + 1
                               , varenv = M.empty:(varenv s)} )

popEnv :: GenerateIR (M.Map VIdent (SVar, T))
popEnv = do
    env <- liftM head $ gets varenv
    modify (\s -> s { levelCounter = (levelCounter s) - 1
                    , varenv = tail (varenv s) })
    return env

generateIR_Program :: Program T -> GenerateIR ()
generateIR_Program (Prog t topdefs) = do
    mapM_ declareTopDef topdefs
    mapM_ generateIR_TopDef topdefs
    generateIR_lambda
    classNames <- liftM M.keys $ gets classInfo
    mapM_ generateIR_Destructor classNames
    mapM_ generateIR_Constructor classNames

generateIR_lambda :: GenerateIR ()
generateIR_lambda = do
    ls <- gets lambdas
    case ls of
      [] -> return ()
      ((t, name, args, stmt):ls) -> do
                                modify (\s -> s { lambdas = tail (lambdas s) })
                                generateIR_TopDef (FnDef (Void ()) t name args (Bl (Void ()) [stmt]))
                                generateIR_lambda

generateIR_TopDef :: TopDef T -> GenerateIR ()
generateIR_TopDef (FnDef _ t name args bl) = do
    modify (\s -> s { currentOutput = [] } )
    emitIR (IR_Label name)
    pushEmptyEnv
    mapM_ declareArg args
    generateIR_Stmt (BStmt (Void ()) bl)
    void popEnv
    modify (\s -> s { output = M.insert name ((reverse . currentOutput) s) (output s) } )
generateIR_TopDef (ClDef _ _ _) = return ()

declareArg :: Argument T -> GenerateIR ()
declareArg (Arg _ t name) = do
    var <- declareVar (fmap (const ()) t) name
    emitIR (IR_Argument var)

destuctorName :: CIdent -> VIdent
destuctorName cls = VIdent ("__" ++ show cls ++ "_destructor")

constructorName :: CIdent -> VIdent
constructorName cls = VIdent ("__" ++ show cls ++ "_constructor")

isFreeable :: T -> Bool
isFreeable (Int ()) = False
isFreeable (Bool ()) = False
isFreeable _ = True

decreaseRefCount :: ValIR -> GenerateIR ValIR
decreaseRefCount x = do
    cnt <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_MemRead t x)
    newcnt <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp ISub t cnt (IntIR 1 4))
    emitIR (IR_MemSave x newcnt 4)
    return newcnt

releaseObject :: T -> ValIR -> GenerateIR ()
releaseObject (Int ()) _ = return ()
releaseObject (Bool ()) _ = return ()
releaseObject (Null ()) _ = return ()
releaseObject t@(Class _ cls) x =
    if L.isPrefixOf "env__class__" (show cls)
       then return ()
       else let f = LabelIR (destuctorName cls)
             in emitIR (IR_VoidCall f [x])
releaseObject t@(Array _ ty) x = do
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
    refcnt <- decreaseRefCount x
    emitIR (IR_CondJump refcnt IR.NEQ (IntIR 0 4) l)
    when (isFreeable ty) (do
            arr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x (IntIR 8 8))
            size' <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x (IntIR 4 8))
            size <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_MemRead t size')
            i <- emitIR_ToTemp 4 (\t -> IR_Ass t (IntIR 0 4))
            l1 <- getFreshLabel
            l2 <- getFreshLabel
            emitIR (IR_Jump l2)
            emitIR (IR_Label l1)
            pos <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IMul t (VarIR i) (IntIR (sizeOf ty) 4))
            y' <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t arr pos)
            y <- liftM VarIR $ emitIR_ToTemp (sizeOf ty) (\t -> IR_MemRead t y')
            releaseObject ty y
            i' <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IAdd t (VarIR i) (IntIR 1 4))
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
    fPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x (IntIR 20 8))
    f <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_MemRead t fPtr)
    cPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x (IntIR 12 8))
    c <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_MemRead t cPtr)
    emitIR (IR_VoidCall f [c])
    emitIR (IR_VoidCall (LabelIR (VIdent "freeMemory")) [x])
    emitIR (IR_Label l)

acquireObject :: T -> ValIR -> GenerateIR ()
acquireObject (Int ()) _ = return ()
acquireObject (Bool ()) _ = return ()
acquireObject (Null ()) _ = return ()
acquireObject t x = do
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
    cnt <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_MemRead t x)
    newcnt <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IAdd t cnt (IntIR 1 4))
    emitIR (IR_MemSave x newcnt 4)
    emitIR (IR_Label l)

decrementRefCount :: T -> ValIR -> GenerateIR ()
decrementRefCount (Int ()) _ = return ()
decrementRefCount (Bool ()) _ = return ()
decrementRefCount (Null ()) _ = return ()
decrementRefCount t x = do
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.EQU (IntIR 0 8) l)
    decreaseRefCount x
    emitIR (IR_Label l)


generateIR_Destructor :: CIdent -> GenerateIR ()
generateIR_Destructor cls = do
    let fName = destuctorName cls
    modify (\s -> s { currentOutput = [] })
    emitIR (IR_Label fName)
    pushEmptyEnv
    let varName = VIdent "obj"
    var <- declareVar (Class () cls) varName
    let x = VarIR var
    emitIR (IR_Argument var)
    l <- getFreshLabel
    emitIR (IR_CondJump x IR.NEQ (IntIR 0 8) l)
    emitIR IR_VoidReturn
    emitIR (IR_Label l)
    when (not ((L.isPrefixOf "env__class__") (show cls)))
        (do
         cnt <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_MemRead t x)
         newcnt <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp ISub t cnt (IntIR 1 4))
         emitIR (IR_MemSave x newcnt 4)
         l <- getFreshLabel
         emitIR (IR_CondJump newcnt IR.EQU (IntIR 0 4) l)
         emitIR IR_VoidReturn
         emitIR (IR_Label l)
        )
    info <- liftM (M.! cls) $ gets classInfo
    mapM_ (freeField info x) (M.keys (fieldSize info))
    let free = LabelIR (VIdent "freeMemory")
    emitIR (IR_VoidCall free [x])
    emitIR IR_VoidReturn
    void popEnv
    modify (\s -> s { output = M.insert fName ((reverse . currentOutput) s) (output s) } )

freeField :: ClassInfo -> ValIR -> VIdent -> GenerateIR ()
freeField info obj name = do
    let offset = (fieldOffset info) M.! name
    let t = (fieldType info) M.! name
    x' <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t obj (IntIR offset 8))
    x <- liftM VarIR $ emitIR_ToTemp (sizeOf t) (\t -> IR_MemRead t x')
    releaseObject t x


generateIR_Constructor :: CIdent -> GenerateIR ()
generateIR_Constructor cls = do
    let fName = constructorName cls
    modify (\s -> s { currentOutput = [] })
    emitIR (IR_Label fName)
    pushEmptyEnv
    size <- liftM (classSize . (M.! cls)) $ gets classInfo
    v <- generateIR_Expr_Alloc (EInt (Int ()) (toInteger size))
    emitIR (IR_MemSave v (IntIR 0 4) 4)
    info <- liftM (M.! cls) $ gets classInfo
    mapM_ (initField info v) (M.keys (fieldSize info))
    emitIR (IR_Return v)
    void popEnv
    modify (\s -> s { output = M.insert fName ((reverse . currentOutput) s) (output s) } )

initField :: ClassInfo -> ValIR -> VIdent -> GenerateIR ()
initField info obj name = do
    let offset = (fieldOffset info) M.! name
    let t = (fieldType info) M.! name
    let v = defaultValue t
    x' <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t obj (IntIR offset 8))
    emitIR (IR_MemSave x' v (sizeOf t))

releaseWholeStack :: GenerateIR ()
releaseWholeStack = do
    envs <- gets varenv
    let m env = mapM_ (\(x, t) -> releaseObject t (VarIR x)) (M.elems env)
    mapM_ m envs

generateIR_Stmt :: Stmt T -> GenerateIR ()
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
    temp <- emitIR_ToTemp 4 (\t -> IR_BinOp IAdd t value (IntIR 1 4))
    f (VarIR temp)
generateIR_Stmt (Decr _ expr) = do
    value <- generateIR_Expr expr
    f <- generateIR_LExpr expr
    temp <- emitIR_ToTemp 4 (\t -> IR_BinOp ISub t value (IntIR 1 4))
    f (VarIR temp)
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
generateIR_Stmt (SExp _ expr) = do
    value <- generateIR_Expr expr
    return ()

generateIR_Decl :: T -> Item T -> GenerateIR ()
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


generateIR_Expr :: Expr T -> GenerateIR ValIR
generateIR_Expr (ETypedExpr _ _ expr) = generateIR_Expr expr
generateIR_Expr (EInt _ n) = return (IntIR (fromInteger n) 4)
generateIR_Expr (ETrue _) = return (IntIR 1 1)
generateIR_Expr (EFalse _) = return (IntIR 0 1)
generateIR_Expr (ENull _) = return (IntIR 0 8)
generateIR_Expr (EVar _ name) = liftM VarIR $ getVar name
generateIR_Expr (EField _ expr field) =
    case exprType expr of
      Class _ cls -> do
            x <- generateIR_Expr expr
            offset <- liftM ((M.! field) . fieldOffset . (M.! cls)) $ gets classInfo
            size <- liftM ((M.! field) . fieldSize . (M.! cls)) $ gets classInfo
            y <- emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x (IntIR offset 8))
            z <- emitIR_ToTemp size (\t -> IR_MemRead t (VarIR y))
            return (VarIR z)
      Array _ _ -> do -- accessing length of array
            x <- generateIR_Expr expr
            x' <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IAdd t x (IntIR 4 8))
            liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_MemRead t x')
      _ -> fail "error"
generateIR_Expr expr@(EArrAcc type_ arr index) = do
    arrV <- generateIR_Expr arr
    ind <- generateIR_Expr index
    let size = sizeOf type_
    arrV' <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t arrV (IntIR 8 8))
    v' <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IMul t ind (IntIR size 4))
    loc <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t arrV' v')
    liftM VarIR $ emitIR_ToTemp size (\t -> IR_MemRead t loc)
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
generateIR_Expr (EObjNew t cls) =
    let f = LabelIR (constructorName cls)
     in liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_Call t f [])
generateIR_Expr (EArrNew tt t expr) = do
    let n = sizeOf t
    v1 <- generateIR_Expr expr
    v2 <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IMul t v1 (IntIR n 4))
    v3 <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IAdd t v2 (IntIR 8 4))
    let f = LabelIR (VIdent "allocMemory")
    ptr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_Call t f [v3])
    emitIR (IR_MemSave ptr (IntIR 0 4) 4)
    t1 <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t ptr (IntIR 4 8))
    emitIR (IR_MemSave t1 v1 4)
    let initVal = defaultValue t
    i <- emitIR_ToTemp 4 (\t -> IR_Ass t (IntIR 0 4))
    p' <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t ptr (IntIR 8 8))
    lLoop <- getFreshLabel
    lEnd <- getFreshLabel
    emitIR (IR_Jump lEnd)
    emitIR (IR_Label lLoop)
    p1 <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IMul t (VarIR i) (IntIR n 4))
    p2 <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t p' p1)
    emitIR (IR_MemSave p2 initVal n)
    i' <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IAdd t (VarIR i) (IntIR 1 4))
    emitIR (IR_Ass i i')
    emitIR (IR_Label lEnd)
    emitIR (IR_CondJump (VarIR i) IR.LTH v1 lLoop)
    return ptr
generateIR_Expr e@(ELambda ty args stmt) = do
    let freeVars = S.toList (getFreeVars (Ret (Void ()) e))
    let initInfo = ClassInfo { classSize = 4, fieldSize = M.empty, fieldOffset = M.empty, fieldType = M.empty }
    info <- foldM buildOffsets initInfo freeVars
    lambdaN <- gets lambdaCounter
    modify (\s -> s { lambdaCounter = lambdaN + 1 })
    let envName = CIdent ("env__class__" ++ show lambdaN)
    let env = VIdent ("__env__" ++ show lambdaN)
    let lambdaName = VIdent ("__lambda__" ++ show lambdaN)
    modify (\s -> s { classInfo = M.insert envName info (classInfo s) })
    v <- generateIR_Expr_Alloc (EInt (Int ()) 28)
    emitIR (IR_MemSave v (IntIR 0 4) 4)
    lPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t v (IntIR 4 8))
    emitIR (IR_MemSave lPtr (LabelIR lambdaName) 8)
    v1 <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t v (IntIR 12 8))
    v2 <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_Call t (LabelIR (constructorName envName)) [])
    mapM_ (setupEnv info v2) freeVars
    emitIR (IR_MemSave v1 v2 8)
    lPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t v (IntIR 20 8))
    let destrName = destuctorName envName
    emitIR (IR_MemSave lPtr (LabelIR destrName) 8)
    let envType = Class (Void ()) envName
    let stmt' = substitute envType env (S.fromList freeVars) stmt
    let tOut = case ty of { Fun _ t _ -> fmap (const (Void ())) t; _ -> Void (Void ()) }
    let envArg = Arg (Void ()) envType env
    addLambda tOut lambdaName (args ++ [envArg]) stmt'
    return v

setupEnv :: ClassInfo -> ValIR -> VIdent -> GenerateIR ()
setupEnv info v x = do
    var <- liftM VarIR $ getVar x
    let offset = (fieldOffset info) M.! x
    let size = (fieldSize info) M.! x
    let ty = (fieldType info) M.! x
    t <- liftM VarIR $ emitIR_ToTemp size (\t -> IR_BinOp IAdd t v (IntIR offset 8))
    acquireObject ty var
    emitIR (IR_MemSave t var size)

generateIR_Expr_Alloc :: Expr T -> GenerateIR ValIR
generateIR_Expr_Alloc expr = do
    let fname = LabelIR (VIdent "allocMemory")
    v <- generateIR_Expr expr
    liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_Call t fname [v])

buildOffsets :: ClassInfo -> VIdent -> GenerateIR ClassInfo
buildOffsets info x = do
    t <- getVarType x
    let size = sizeOf t
    return (ClassInfo { classSize = (classSize info) + size
                      , fieldSize = M.insert x size (fieldSize info)
                      , fieldOffset = M.insert x (classSize info) (fieldOffset info)
                      , fieldType = M.insert x t (fieldType info)
                      })


generateIR_UnOp :: UnOp -> Expr T -> GenerateIR ValIR
generateIR_UnOp op expr = do
    v <- generateIR_Expr expr
    let size = sizeOf (exprType expr)
    liftM VarIR $ emitIR_ToTemp size (\t -> IR_UnOp op t v)

generateIR_BinOp :: BinOp -> Expr T -> Expr T -> GenerateIR ValIR
generateIR_BinOp op expr1 expr2 = do
    v1 <- generateIR_Expr expr1
    v2 <- generateIR_Expr expr2
    let size = sizeOf (exprType expr1)
    liftM VarIR $ emitIR_ToTemp size (\t -> IR_BinOp op t v1 v2)

generateIR_LExpr :: Expr T -> GenerateIR (ValIR -> GenerateIR ())
generateIR_LExpr (EVar _ name) = do
    var <- getVar name
    return (\v -> emitIR (IR_Ass var v))
generateIR_LExpr (EArrAcc type_ arr expr) = do
    x <- generateIR_Expr arr
    ind <- generateIR_Expr expr
    let size = sizeOf type_
    x' <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x (IntIR 8 8))
    v' <- liftM VarIR $ emitIR_ToTemp 4 (\t -> IR_BinOp IMul t ind (IntIR size 4))
    loc <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x' v')
    return (\v -> emitIR (IR_MemSave loc v size))
generateIR_LExpr (EField _ expr field) =
    case exprType expr of
      Class _ cls -> do
            x <- generateIR_Expr expr
            offset <- liftM ((M.! field) . fieldOffset . (M.! cls)) $ gets classInfo
            size <- liftM ((M.! field) . fieldSize . (M.! cls)) $ gets classInfo
            y <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t x (IntIR offset 8))
            return (\v -> emitIR (IR_MemSave y v size))
      _ -> fail "error"

generateIR_AppExpr :: Expr T -> T -> GenerateIR ([ValIR] -> GenerateIR ValIR)
generateIR_AppExpr (EVar _ name) ty = do
    v <- getMaybeVar name
    case v of
      Just var -> return (\xs -> do
                                 let v = VarIR var
                                 fPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t v (IntIR 4 8))
                                 f <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_MemRead t fPtr)
                                 temp <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t v (IntIR 12 8))
                                 envPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_MemRead t temp)
                                 liftM VarIR $ emitIR_ToTemp (sizeOf ty) (\t -> IR_Call t f (xs ++ [envPtr]))
                         )
      Nothing -> return (\xs -> liftM VarIR $ emitIR_ToTemp (sizeOf ty) (\t -> IR_Call t (LabelIR name) xs))
generateIR_AppExpr fExpr ty = do
    v <- generateIR_Expr fExpr
    return (\xs -> do
                   fPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t v (IntIR 4 8))
                   f <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_MemRead t fPtr)
                   temp <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_BinOp IAdd t v (IntIR 12 8))
                   envPtr <- liftM VarIR $ emitIR_ToTemp 8 (\t -> IR_MemRead t temp)
                   liftM VarIR $ emitIR_ToTemp (sizeOf ty) (\t -> IR_Call t f (xs ++ [envPtr]))
           )

generateIR_JumpExpr :: Expr T -> VIdent -> VIdent -> GenerateIR ()
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

generateIR_BoolExpr :: Expr T -> GenerateIR ValIR
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
exprType (EObjNew t _) = t
exprType (EArrNew t _ _) = t
exprType (ELambda t _ _) = t

