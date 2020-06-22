module StaticCheck (
    staticCheck
                   ) where

import AbsSPL
import CheckM
import ErrM
import LexSPL
import Token
import Type

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

self :: VIdent
self = VIdent "self"

staticCheck :: Program Pos -> Err (Program T)
staticCheck program = runCheckM (staticCheck_Program program)

assert :: Pos -> Bool -> String -> CheckM ()
assert pos b msg = unless b (errorMsg pos msg)

assertTypesEqual :: Pos -> T -> T -> CheckM ()
assertTypesEqual _ (Class _ _) (Null _) = return ()
assertTypesEqual _ (Null _) (Class _ _) = return ()
assertTypesEqual pos t1 t2 = assert pos (t1 == t2) msg
    where msg = "Types don't match: " ++ show t1 ++ " != " ++ show t2

tryGetArrayType :: Pos -> T -> CheckM T
tryGetArrayType _ (Array _ t) = return t
tryGetArrayType pos _ = errorMsg pos "Type is not array"

tryGetFunctionType :: Pos -> T -> CheckM (T, [T])
tryGetFunctionType _ (Fun _ retType argTypes) = return (retType, argTypes)
tryGetFunctionType pos  _ = errorMsg pos "Type is not function"

tryGetClassName :: Pos -> T -> CheckM CIdent
tryGetClassName _ (Class _ cls) = return cls
tryGetClassName pos _ = errorMsg pos "Type is not a class"

unaryOpType :: UnaryOp a -> T
unaryOpType (Neg _)    = intT
unaryOpType (Not _)    = boolT
unaryOpType (BitNot _) = intT

mulOpType :: MulOp a -> T
mulOpType _ = intT

addOpType :: AddOp a -> T
addOpType _ = intT

relOpType :: RelOp a -> T
relOpType _ = intT

staticCheck_Program :: Program Pos -> CheckM (Program T)
staticCheck_Program (Prog _ topdefs) = do
    buildClassGraph topdefs
    mapM_ declareTopDef topdefs
    topdefs' <- mapM staticCheck_TopDef topdefs
    return (Prog voidT topdefs')

staticCheck_TopDef :: TopDef Pos -> CheckM (TopDef T)
staticCheck_TopDef (FnDef _ t name args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            modify (\s -> s { rettype = (toUnit t) })
            mapM_ declareArg args
            stmts' <- mapM staticCheck_Stmt stmts
            let t' = toVoid t
            let args' = fmap toVoid args
            let stmts'' = stmts' ++ if t' == (toVoid voidT) then [VRet voidT] else []
            return (FnDef voidT t' name args' (Bl voidT stmts''))
staticCheck_TopDef t@(ClassDef _ cls extends args) = doWithSavedEnv m
    where m = do
            modify (\s -> s { currentClass = Just cls })
            args' <- mapM staticCheck_ClassElem args
            modify (\s -> s { currentClass = Nothing })
            return (ClassDef voidT cls (toVoid extends) args')

staticCheck_ClassElem :: ClassElem Pos -> CheckM (ClassElem T)
staticCheck_ClassElem e@(Field _ _ _) = return (toVoid e)
staticCheck_ClassElem (Method _ t name args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            modify (\s -> s { rettype = (toUnit t) })
            mapM_ declareArg args
            maybe_cls <- gets currentClass
            let (Just cls) = maybe_cls
            declareArg (Arg Nothing (Class Nothing cls) self)
            stmts' <- mapM staticCheck_Stmt stmts
            return (Method voidT (toVoid t) name (map toVoid args) (Bl voidT stmts'))
staticCheck_ClassElem (Constr _ args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            maybe_cls <- gets currentClass
            let (Just cls) = maybe_cls
            let t = classT cls
            modify (\s -> s { rettype = t })
            mapM_ declareArg args
            let expr = EVar Nothing self
            let stmts' = map (changeVRet expr) (stmts ++ [VRet Nothing])
            declareArg (Arg Nothing (Class Nothing cls) self)
            stmts'' <- mapM staticCheck_Stmt stmts'
            return (Constr voidT (map toVoid args) (Bl voidT stmts''))

changeVRet :: Expr a -> Stmt a -> Stmt a
changeVRet expr stmt =
    let f = changeVRet expr
     in case stmt of
          BStmt p1 (Bl p2 sx) -> BStmt p1 (Bl p2 (map f sx))
          VRet p -> Ret p expr
          Cond p e s -> Cond p e (f s)
          CondElse p e s1 s2 -> CondElse p e (f s1) (f s2)
          While p e s -> While p e (f s)
          ForUp p name e1 e2 e3 s -> ForUp p name e1 e2 e3 (f s)
          ForDown p name e1 e2 e3 s -> ForDown p name e1 e2 e3 (f s)
          ForEach p name e s -> ForEach p name e (f s)
          _ -> stmt


staticCheck_Stmt :: Stmt Pos -> CheckM (Stmt T)
staticCheck_Stmt (Empty _) = return (Empty voidT)
staticCheck_Stmt (BStmt _ (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            stmts' <- mapM staticCheck_Stmt stmts
            return (BStmt voidT (Bl voidT stmts'))
staticCheck_Stmt (Decl _ t items) = do
    items' <- mapM (staticCheck_Decl (toUnit t)) items
    let t' = toVoid t
    return (Decl voidT t' items')
staticCheck_Stmt (Ass pos e1 e2) = do
    (e1', t1) <- staticCheck_LExpr e1
    (e2', t2) <- staticCheck_Expr e2
    assertCanAssign pos t1 t2
    return (Ass voidT e1' e2')
staticCheck_Stmt (Incr pos e) = do
    (e', t) <- staticCheck_LExpr e
    assertCanAssign pos intT t
    return (Incr voidT e')
staticCheck_Stmt (Decr pos e) = do
    (e', t) <- staticCheck_LExpr e
    assertCanAssign pos intT t
    return (Decr voidT e')
staticCheck_Stmt (Ret pos e) = do
    (e', t) <- staticCheck_Expr e
    t1 <- getReturnType
    assertCanAssign pos t1 t
    return (Ret voidT e')
staticCheck_Stmt (VRet pos) = do
    t <- getReturnType
    assertCanAssign pos t voidT
    return (VRet voidT)
staticCheck_Stmt (Cond pos e s) = do
    (e', t) <- staticCheck_Expr e
    assertCanAssign pos boolT t
    s' <- staticCheck_Stmt s
    return (Cond voidT e' s')
staticCheck_Stmt (CondElse pos e s1 s2) = do
    (e', t) <- staticCheck_Expr e
    assertCanAssign pos boolT t
    s1' <- staticCheck_Stmt s1
    s2' <- staticCheck_Stmt s2
    return (CondElse voidT e' s1' s2')
staticCheck_Stmt (While pos e s) = do
    (e', t) <- staticCheck_Expr e
    assertCanAssign pos boolT t
    s' <- staticCheck_Stmt s
    return (While voidT e' s')
staticCheck_Stmt (ForUp pos name e1 e2 e3 s) = do
    (e1', t1) <- staticCheck_Expr e1
    (e2', t2) <- staticCheck_Expr e2
    (e3', t3) <- staticCheck_Expr e3
    assertCanAssign (getPos_Expr e1) intT t1
    assertCanAssign (getPos_Expr e2) intT t2
    assertCanAssign (getPos_Expr e3) intT t3
    let m = do
            declareVar Nothing name intT
            staticCheck_Stmt s
    s' <- doWithSavedEnv m
    return (ForUp voidT name e1' e2' e3' s')
staticCheck_Stmt (ForDown pos name e1 e2 e3 s) = do
    (e1', t1) <- staticCheck_Expr e1
    (e2', t2) <- staticCheck_Expr e2
    (e3', t3) <- staticCheck_Expr e3
    assertCanAssign (getPos_Expr e1) intT t1
    assertCanAssign (getPos_Expr e2) intT t2
    assertCanAssign (getPos_Expr e3) intT t3
    let m = do
            declareVar Nothing name intT
            staticCheck_Stmt s
    s' <- doWithSavedEnv m
    return (ForDown voidT name e1' e2' e3' s')
staticCheck_Stmt (ForEach pos name e s) = do
    (e', t) <- staticCheck_Expr e
    elemType <- tryGetArrayType (getPos_Expr e) t
    let m = do
            declareVar Nothing name elemType
            staticCheck_Stmt s
    s' <- doWithSavedEnv m
    return (ForEach voidT name e' s')
staticCheck_Stmt (SExp _ e) = do
    (e', _) <- staticCheck_Expr e
    return (SExp voidT e')


staticCheck_Decl :: T -> Item Pos -> CheckM (Item T)
staticCheck_Decl t (NoInit pos name) = do
    declareVar pos name t
    return (NoInit voidT name)
staticCheck_Decl t (Init pos name expr) = do
    (expr', t') <- staticCheck_Expr expr
    assertCanAssign pos t t'
    declareVar pos name t
    return (Init voidT name expr')

staticCheck_LExpr :: Expr Pos -> CheckM (Expr T, T)
staticCheck_LExpr e@(EVar _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EField _ _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EArrAcc _ _ _) = staticCheck_Expr e
staticCheck_LExpr e = errorMsg (getPos_Expr e) "Wrong lexpr"


staticCheck_Expr :: Expr Pos -> CheckM (Expr T, T)
staticCheck_Expr (EInt _ n) = return (EInt intT n, intT)
staticCheck_Expr (ETrue _) = return (ETrue boolT, boolT)
staticCheck_Expr (EFalse _) = return (EFalse boolT, boolT)
staticCheck_Expr (ENull _) = return (ENull nullT, nullT)
staticCheck_Expr (EVar pos name) = do
    let err = errorMsg pos $ "Variable " ++ show name ++ " not declared"
    info <- liftM (M.!? name) $ gets varEnv
    case info of
      Just i -> let t = varType i
                 in return (EVar t name, t)
      Nothing -> do
                maybe_cls <- gets currentClass
                case maybe_cls of
                  Just cls -> do
                            info <- liftM (!!! cls) $ gets classEnv
                            if M.member name (fieldTypes info)
                               then let t = (fieldTypes info) !!! name
                                      in return (EField t (EVar (classT cls) self) name, t)
                               else do
                                   unless (M.member name (methodTypes info)) err
                                   let t = (methodTypes info) !!! name
                                   return (EField t (EVar (classT cls) self) name, t)
                  Nothing -> err
staticCheck_Expr (EField pos expr field) = do
    let err = errorMsg pos $ "Field " ++ show field ++ " doesn't exist"
    (expr', t') <- staticCheck_Expr expr
    case t' of
      Class _ cls -> do
                     info <- getClassInfo pos cls
                     if M.member field (fieldTypes info)
                        then let t = (fieldTypes info) !!! field
                              in return (EField t expr' field, t)
                        else do
                            unless (M.member field (methodTypes info)) err
                            let t = (methodTypes info) !!! field
                            return (EField t expr' field, t)
      Array _ _ -> do
                   assert pos (field == VIdent "length") "Expected object"
                   return (EField intT expr' field, intT)
      _ -> errorMsg pos "Expected object"
staticCheck_Expr (EArrAcc pos arrExpr indexExpr) = do
    (arrExpr', arrT) <- staticCheck_Expr arrExpr
    (indexExpr', indexT) <- staticCheck_Expr indexExpr
    assert pos (indexT == intT) "Index is not int"
    t <- tryGetArrayType pos arrT
    return (EArrAcc t arrExpr' indexExpr', t)
staticCheck_Expr (EApp pos funExpr argExprs) = do
    (funExpr', funT) <- staticCheck_Expr funExpr
    (retT, argTs) <- tryGetFunctionType pos funT
    assert pos ((length argExprs) == (length argTs)) $ "Wrong number of arguments"
    (argExprs', ts') <- liftM unzip $ mapM staticCheck_Expr argExprs
    let m (pos, t1, t2) = assertCanAssign pos t1 t2
    mapM_ m (zip3 (map getPos_Expr argExprs) argTs ts')
    return (EApp retT funExpr' argExprs', retT)
staticCheck_Expr (EUnaryOp pos op expr) = do
    (expr', t) <- staticCheck_Expr expr
    assertCanAssign pos (unaryOpType op) t
    let op' = toVoid op
    return (EUnaryOp t op' expr', t)
staticCheck_Expr (EMul pos expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertCanAssign pos intT t1
    assertCanAssign pos intT t2
    let op' = toVoid op
    return (EMul intT expr1' op' expr2', intT)
staticCheck_Expr (EAdd pos expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertCanAssign pos intT t1
    assertCanAssign pos intT t2
    let op' = toVoid op
    return (EAdd intT expr1' op' expr2', intT)
staticCheck_Expr (ERel pos expr1 (EQU _) expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual pos t1 t2
    let op' = EQU voidT
    return (ERel boolT expr1' op' expr2', boolT)
staticCheck_Expr (ERel pos expr1 (NE _) expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual pos t1 t2
    let op' = NE voidT
    return (ERel boolT expr1' op' expr2', boolT)
staticCheck_Expr (ERel pos expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual pos intT t1
    assertTypesEqual pos intT t2
    let op' = toVoid op
    return (ERel boolT expr1' op' expr2', boolT)
staticCheck_Expr (EAnd pos expr1 expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual pos boolT t1
    assertTypesEqual pos boolT t2
    return (EAnd boolT expr1' expr2', boolT)
staticCheck_Expr (EOr pos expr1 expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual pos boolT t1
    assertTypesEqual pos boolT t2
    return (EOr boolT expr1' expr2', boolT)
staticCheck_Expr (EObjNew pos cls exprs) = do
    (exprs', ts') <- liftM unzip $ mapM staticCheck_Expr exprs
    let constrT = funT voidT ts'
    constrs <- liftM constructors $ getClassInfo pos cls
    g <- gets classGraph
    let b = any (\ts -> isSubtypeList g ts ts') (S.toList constrs)
    let err = errorMsg pos $ "Wrong constructor: " ++ show ts'
    unless b err
    let t = classT cls
    return (EObjNew t cls exprs', t)
staticCheck_Expr (EArrNew pos t expr) = do
    (expr', t') <- staticCheck_Expr expr
    assertTypesEqual pos intT t'
    let newt = Array () (toUnit t)
    return (EArrNew newt (toVoid t) expr', newt)
staticCheck_Expr (ELambda pos args stmt) = doWithSavedEnv m
    where m = do
            tRetPrev <- gets rettype
            mapM_ (\(Arg pos' t x) -> declareVar pos' x (toUnit t)) args
            tOut <- staticCheck_getRetExpr stmt
            modify (\s -> s { rettype = tOut })
            stmt' <- staticCheck_Stmt stmt
            let ts = map (\(Arg _ t _) -> toUnit t) args
            let t' = Fun () tOut ts
            let args' = fmap toVoid args
            modify (\s -> s { rettype = tRetPrev })
            return (ELambda t' args' stmt', t')

staticCheck_getRetExpr :: Stmt Pos -> CheckM T
staticCheck_getRetExpr stmt = do
    ts <- staticCheck_RetType stmt
    case ts of
      [] -> return (Void ())
      (x:xs) -> do
                mapM_ (assertTypesEqual Nothing x) xs
                return x

staticCheck_RetType :: Stmt Pos -> CheckM [T]
staticCheck_RetType (BStmt _ (Bl _ stmts)) = do
    liftM concat $ mapM staticCheck_RetType stmts
staticCheck_RetType (Decl _ t items) = do
    mapM_ (staticCheck_Decl (toUnit t)) items
    return []
staticCheck_RetType (Ret _ e) = do
    (_, t) <- staticCheck_Expr e
    return [t]
staticCheck_RetType (VRet _) = return [Void ()]
staticCheck_RetType (Cond _ e s) = staticCheck_RetType s
staticCheck_RetType (CondElse _ e s1 s2) = do
    t1 <- staticCheck_RetType s1
    t2 <- staticCheck_RetType s2
    return (t1 ++ t2)
staticCheck_RetType (While _ e s) = staticCheck_RetType s
staticCheck_RetType _ = return []

getPos_Expr :: Expr Pos -> Pos
getPos_Expr (ENull pos) = pos
getPos_Expr (EInt pos _) = pos
getPos_Expr (ETrue pos) = pos
getPos_Expr (EFalse pos) = pos
getPos_Expr (EVar pos _) = pos
getPos_Expr (EField pos _ _) = pos
getPos_Expr (EArrAcc pos _ _) = pos
getPos_Expr (EApp pos _ _) = pos
getPos_Expr (EUnaryOp pos _ _) = pos
getPos_Expr (EMul pos _ _ _) = pos
getPos_Expr (EAdd pos _ _ _) = pos
getPos_Expr (ERel pos _ _ _) = pos
getPos_Expr (EAnd pos _ _) = pos
getPos_Expr (EOr pos _ _) = pos
getPos_Expr (EObjNew pos _ _) = pos
getPos_Expr (EArrNew pos _ _) = pos
getPos_Expr (ELambda pos _ _) = pos
