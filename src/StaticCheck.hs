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

staticCheck_Program :: Program Pos -> CheckM (Program T)
staticCheck_Program (Prog _ topdefs) = do
    buildClassGraph topdefs
    mapM_ declareTopDef topdefs
    topdefs' <- mapM staticCheck_TopDef topdefs
    return (Prog voidT topdefs')

declareTopDef :: TopDef Pos -> CheckM ()
declareTopDef (FnDef pos t name args _) = do
    t' <- liftM toUnit $ staticCheck_Type t
    args' <- mapM staticCheck_Argument args
    let ts' = map getArgType args'
    declareVar pos name (funT t' ts')
declareTopDef (ClassDef pos cls (NoExtends _) args) = do
    (fieldEnv, methodEnv, _, constr') <- foldM addClassElem (M.empty, M.empty, S.empty, S.empty) args
    let constr = if S.null constr' then S.singleton [] else constr'
    let info = ClassInfo { fieldTypes = fieldEnv
                         , methodTypes = methodEnv
                         , extends = Nothing
                         , constructors = constr}
    declareClass pos cls info
declareTopDef (ClassDef pos cls (Extends pos' superCls) args) = do
    assertClassNotDeclared pos cls
    superInfo <- getClassInfo pos' superCls
    let f = fieldTypes superInfo
    let m = methodTypes superInfo
    let acc = (f, m, S.empty, S.empty)
    (fieldEnv, methodEnv, _, constr') <- foldM addClassElem acc args
    let constr = if S.null constr' then S.singleton [] else constr'
    let info = ClassInfo { fieldTypes = fieldEnv
                         , methodTypes = methodEnv
                         , extends = Just superCls
                         , constructors = constr}
    declareClass pos cls info
declareTopDef (TypeDef pos name ty) = do
    t <- liftM toUnit $ staticCheck_Type ty
    modify (\s -> s { typedefs = M.insert name t (typedefs s) })

addClassElem :: (TypeEnv, TypeEnv, S.Set VIdent, S.Set [T]) -> ClassElem Pos -> CheckM (TypeEnv, TypeEnv, S.Set VIdent, S.Set [T])
addClassElem (fieldEnv, methodEnv, usedNames, constrs) (Field pos ty names) = do
    t <- liftM toUnit $ staticCheck_Type ty
    let m (fEnv, set) name = do
                             let err = errorMsg pos $ show name ++ " already declared"
                             when (S.member name set) err
                             when (M.member name fEnv) err
                             when (M.member name methodEnv) err
                             return (M.insert name t fEnv, S.insert name set)
    (fieldEnv', usedNames') <- foldM m (fieldEnv, usedNames) names
    return (fieldEnv', methodEnv, usedNames', constrs)
addClassElem (fieldEnv, methodEnv, usedNames, constrs) (Method pos ty name args _) = do
    t' <- liftM toUnit $ staticCheck_Type ty
    args' <- mapM staticCheck_Argument args
    let ts' = map getArgType args'
    let t = funT t' ts'
    let err = errorMsg pos $ show name ++ " already declared"
    when (S.member name usedNames) err
    when (M.member name fieldEnv) err
    when (M.member name methodEnv) (when (t /= methodEnv !!! name) err)
    let methodEnv' = M.insert name t methodEnv
    let usedNames' = S.insert name usedNames
    return (fieldEnv, methodEnv', usedNames', constrs)
addClassElem (fieldEnv, methodEnv, usedNames, constrs) (Constr pos args _) = do
    args' <- mapM staticCheck_Argument args
    let t = map getArgType args'
    let err = errorMsg pos $ "Redefinition of constructor: " ++ show t
    g <- gets classGraph
    let b = any (\t' -> isSubtypeList g t' t || isSubtypeList g t t') (S.toList constrs)
    when b err
    let constrs' = S.insert t constrs
    return (fieldEnv, methodEnv, usedNames, constrs')

staticCheck_TopDef :: TopDef Pos -> CheckM (TopDef T)
staticCheck_TopDef (FnDef _ t name args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            t' <- staticCheck_Type t
            args' <- mapM staticCheck_Argument args
            modify (\s -> s { rettype = toUnit t' })
            mapM_ declareArg (map (fmap (const Nothing)) args')
            stmts' <- mapM staticCheck_Stmt stmts
            let stmts'' = stmts' ++ if toUnit t' == voidT then [VRet voidT] else []
            return (FnDef voidT t' name args' (Bl voidT stmts''))
staticCheck_TopDef t@(ClassDef _ cls extends args) = doWithSavedEnv m
    where m = do
            modify (\s -> s { currentClass = Just cls })
            args' <- mapM staticCheck_ClassElem args
            modify (\s -> s { currentClass = Nothing })
            return (ClassDef voidT cls (toVoid extends) args')
staticCheck_TopDef t@(TypeDef _ _ _) = return (toVoid t)

staticCheck_Argument :: Argument Pos -> CheckM (Argument T)
staticCheck_Argument (Arg pos t name) = do
    t' <- staticCheck_Type t
    return (Arg voidT t' name)

staticCheck_Type :: Type Pos -> CheckM (Type T)
staticCheck_Type (NamedType pos name) = do
    m <- gets typedefs
    case m M.!? name of
      Nothing -> errorMsg pos $ "Type " ++ show name ++ " not defined"
      Just t -> return (toVoid t)
staticCheck_Type (Array pos t) = do
    t' <- staticCheck_Type t
    return (Array voidT t')
staticCheck_Type (Fun pos t ts) = do
    t' <- staticCheck_Type t
    ts' <- mapM staticCheck_Type ts
    return (Fun voidT t' ts')
staticCheck_Type t = return (toVoid t)


staticCheck_ClassElem :: ClassElem Pos -> CheckM (ClassElem T)
staticCheck_ClassElem (Field _ t names) = do
    t' <- staticCheck_Type t
    return (Field voidT t' names)
staticCheck_ClassElem (Method _ t name args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            t' <- staticCheck_Type t
            args' <- mapM staticCheck_Argument args
            modify (\s -> s { rettype = toUnit t' })
            mapM_ declareArg (map (fmap (const Nothing)) args')
            maybe_cls <- gets currentClass
            let (Just cls) = maybe_cls
            declareArg (Arg Nothing (Class Nothing cls) self)
            stmts' <- mapM staticCheck_Stmt stmts
            return (Method voidT t' name args' (Bl voidT stmts'))
staticCheck_ClassElem (Constr _ args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            args' <- mapM staticCheck_Argument args
            maybe_cls <- gets currentClass
            let (Just cls) = maybe_cls
            let t = classT cls
            modify (\s -> s { rettype = t })
            mapM_ declareArg (map (fmap (const Nothing)) args')
            let expr = EVar Nothing self
            let stmts' = map (changeVRet expr) (stmts ++ [VRet Nothing])
            declareArg (Arg Nothing (Class Nothing cls) self)
            stmts'' <- mapM staticCheck_Stmt stmts'
            return (Constr voidT args' (Bl voidT stmts''))

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
    t' <- staticCheck_Type t
    items' <- mapM (staticCheck_Decl (toUnit t')) items
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
staticCheck_Expr (EChar _ c) = return (EChar charT c, charT)
staticCheck_Expr (EString _ str) = return (EString t str, t)
    where t = arrayT charT
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
    let err2 = errorMsg pos $ "Expected object (not " ++ show t' ++ ")"
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
                   unless (field == VIdent "length") err2
                   return (EField intT expr' field, intT)
      _ -> err2
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
    assertTypeMatchOper pos op t
    t' <- getOutputType op t
    return (EUnaryOp t' op expr', t')
staticCheck_Expr (EBinOp pos expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    t <- getCommonType pos t1 t2
    assertTypeMatchOper pos op t
    t' <- getOutputType op t
    return (EBinOp t' expr1' op expr2', t')
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
    t' <- staticCheck_Type t
    (expr', tExpr) <- staticCheck_Expr expr
    assertTypesEqual pos intT tExpr
    let newt = Array () (toUnit t')
    return (EArrNew newt t' expr', newt)
staticCheck_Expr (ELambda pos args stmt) = doWithSavedEnv m
    where m = do
            args' <- mapM staticCheck_Argument args
            tRetPrev <- gets rettype
            mapM_ (\(Arg _ t x) -> declareVar Nothing x (toUnit t)) args'
            tOut <- staticCheck_getRetExpr pos stmt
            modify (\s -> s { rettype = tOut })
            stmt' <- staticCheck_Stmt stmt
            let ts = map (\(Arg _ t _) -> toUnit t) args'
            let t' = Fun () tOut ts
            modify (\s -> s { rettype = tRetPrev })
            return (ELambda t' args' stmt', t')
staticCheck_Expr (EArray pos exprs) = do
    (exprs', ts) <- liftM unzip $ mapM staticCheck_Expr exprs
    g <- gets classGraph
    case ts of
      [] -> errorMsg pos "Cannot create empty array"
      (x:xs) -> let m Nothing t' = Nothing
                    m (Just t) t' = commonSuperType g t t'
                    commotT = foldl m (Just x) xs
                 in case commotT of
                      Nothing -> errorMsg pos "Cannot find common type"
                      Just t -> return (EArray (arrayT t) exprs', arrayT t)


staticCheck_getRetExpr :: Pos -> Stmt Pos -> CheckM T
staticCheck_getRetExpr pos stmt = do
    g <- gets classGraph
    ts <- staticCheck_RetType stmt
    case ts of
      [] -> return (Void ())
      (x:xs) -> do
                let m Nothing t' = Nothing
                    m (Just t) t' = commonSuperType g t t'
                    commonT = foldl m (Just x) xs
                case commonT of
                  Nothing -> errorMsg pos "Cannot find common type"
                  Just t -> return t

staticCheck_RetType :: Stmt Pos -> CheckM [T]
staticCheck_RetType (BStmt _ (Bl _ stmts)) = do
    liftM concat $ mapM staticCheck_RetType stmts
staticCheck_RetType (Decl _ t items) = do
    t' <- liftM toUnit $ staticCheck_Type t
    mapM_ (staticCheck_Decl t') items
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
getPos_Expr (EChar pos _) = pos
getPos_Expr (EString pos _) = pos
getPos_Expr (EVar pos _) = pos
getPos_Expr (EField pos _ _) = pos
getPos_Expr (EArrAcc pos _ _) = pos
getPos_Expr (EApp pos _ _) = pos
getPos_Expr (EUnaryOp pos _ _) = pos
getPos_Expr (EBinOp pos _ _ _) = pos
getPos_Expr (EObjNew pos _ _) = pos
getPos_Expr (EArrNew pos _ _) = pos
getPos_Expr (ELambda pos _ _) = pos
getPos_Expr (EArray pos _) = pos
