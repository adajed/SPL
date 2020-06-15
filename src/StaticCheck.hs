module StaticCheck (
    staticCheck
                   ) where

import AbsSPL
import CheckM
import ErrM
import LexSPL

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M

staticCheck :: Program Pos -> Err (Program TType)
staticCheck program = runCheckM (staticCheck_Program program)

assert :: Pos -> Bool -> String -> CheckM ()
assert pos b msg = unless b (errorMsg pos msg)

assertTypesEqual :: Pos -> TType -> TType -> CheckM ()
assertTypesEqual _ (Class _ _) (Null _) = return ()
assertTypesEqual _ (Null _) (Class _ _) = return ()
assertTypesEqual pos t1 t2 = assert pos (t1 == t2) msg
    where msg = "Types don't match: " ++ show t1 ++ " != " ++ show t2

tryGetArrayType :: Pos -> TType -> CheckM TType
tryGetArrayType _ (Array _ t) = return t
tryGetArrayType pos _ = errorMsg pos "Type is not array"

tryGetFunctionType :: Pos -> TType -> CheckM (TType, [TType])
tryGetFunctionType _ (Fun _ retType argTypes) = return (retType, argTypes)
tryGetFunctionType pos  _ = errorMsg pos "Type is not function"

tryGetClassName :: Pos -> TType -> CheckM CIdent
tryGetClassName _ (Class _ cls) = return cls
tryGetClassName pos _ = errorMsg pos "Type is not a class"

unaryOpType :: UnaryOp a -> TType
unaryOpType (Neg _)    = intT
unaryOpType (Not _)    = boolT
unaryOpType (BitNot _) = intT

mulOpType :: MulOp a -> TType
mulOpType _ = intT

addOpType :: AddOp a -> TType
addOpType _ = intT

relOpType :: RelOp a -> TType
relOpType _ = intT

staticCheck_Program :: Program Pos -> CheckM (Program TType)
staticCheck_Program (Prog _ topdefs) = do
    mapM_ declareTopDef topdefs
    topdefs' <- mapM staticCheck_TopDef topdefs
    return (Prog voidT topdefs')

staticCheck_TopDef :: TopDef Pos -> CheckM (TopDef TType)
staticCheck_TopDef (FnDef _ t name args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            modify (\s -> s { rettype = (toUnit t) })
            modify (\s -> s { level = (level s) + 1 })
            mapM_ declareArg args
            stmts' <- mapM staticCheck_Stmt stmts
            let t' = toVoid t
            let args' = fmap toVoid args
            modify (\s -> s { level = (level s) - 1 })
            return (FnDef voidT t' name args' (Bl voidT stmts'))
staticCheck_TopDef t@(ClDef _ _ _) = return (toVoid t)

staticCheck_Stmt :: Stmt Pos -> CheckM (Stmt TType)
staticCheck_Stmt (Empty _) = return (Empty voidT)
staticCheck_Stmt (BStmt _ (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            modify (\s -> s { level = (level s) + 1 })
            stmts' <- mapM staticCheck_Stmt stmts
            modify (\s -> s { level = (level s) - 1 })
            return (BStmt voidT (Bl voidT stmts'))
staticCheck_Stmt (Decl _ t items) = do
    items' <- mapM (staticCheck_Decl (toUnit t)) items
    let t' = toVoid t
    return (Decl voidT t' items')
staticCheck_Stmt (Ass pos e1 e2) = do
    (e1', t1) <- staticCheck_LExpr e1
    (e2', t2) <- staticCheck_Expr e2
    assertTypesEqual pos t1 t2
    return (Ass voidT e1' e2')
staticCheck_Stmt (Incr pos e) = do
    (e', t) <- staticCheck_LExpr e
    assertTypesEqual pos intT t
    return (Incr voidT e')
staticCheck_Stmt (Decr pos e) = do
    (e', t) <- staticCheck_LExpr e
    assertTypesEqual pos intT t
    return (Decr voidT e')
staticCheck_Stmt (Ret pos e) = do
    (e', t) <- staticCheck_Expr e
    t1 <- getReturnType
    assertTypesEqual pos t1 t
    return (Ret voidT e')
staticCheck_Stmt (VRet pos) = do
    assertTypesEqual pos voidT =<< getReturnType
    return (VRet voidT)
staticCheck_Stmt (Cond pos e s) = do
    (e', t) <- staticCheck_Expr e
    assertTypesEqual pos boolT t
    s' <- staticCheck_Stmt s
    return (Cond voidT e' s')
staticCheck_Stmt (CondElse pos e s1 s2) = do
    (e', t) <- staticCheck_Expr e
    assertTypesEqual pos boolT t
    s1' <- staticCheck_Stmt s1
    s2' <- staticCheck_Stmt s2
    return (CondElse voidT e' s1' s2')
staticCheck_Stmt (While pos e s) = do
    (e', t) <- staticCheck_Expr e
    assertTypesEqual pos boolT t
    s' <- staticCheck_Stmt s
    return (While voidT e' s')
staticCheck_Stmt (SExp _ e) = do
    (e', _) <- staticCheck_Expr e
    return (SExp voidT e')


staticCheck_Decl :: TType -> Item Pos -> CheckM (Item TType)
staticCheck_Decl t (NoInit pos name) = do
    declareVar pos name t
    return (NoInit voidT name)
staticCheck_Decl t (Init pos name expr) = do
    (expr', t') <- staticCheck_Expr expr
    assertTypesEqual pos t t'
    declareVar pos name t
    return (Init voidT name expr')

staticCheck_LExpr :: Expr Pos -> CheckM (Expr TType, TType)
staticCheck_LExpr e@(EVar _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EField _ _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EArrAcc _ _ _) = staticCheck_Expr e
staticCheck_LExpr e = errorMsg (getPos_Expr e) "Wrong lexpr"


staticCheck_Expr :: Expr Pos -> CheckM (Expr TType, TType)
staticCheck_Expr (EInt _ n) = return (EInt intT n, intT)
staticCheck_Expr (ETrue _) = return (ETrue boolT, boolT)
staticCheck_Expr (EFalse _) = return (EFalse boolT, boolT)
staticCheck_Expr (ENull _) = return (ENull nullT, nullT)
staticCheck_Expr (EVar pos name) = do
    t <- getVariableType pos name
    return (EVar t name, t)
staticCheck_Expr (EField pos expr field) = do
    (expr', t') <- staticCheck_Expr expr
    case t' of
      Class _ cls -> do
                     fields <- liftM fieldTypes $ getClassInfo pos cls
                     assert pos (M.member field fields) "Field doesn't exist"
                     let t = fields M.! field
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
    assert pos ((length argExprs) == (length argTs)) "Wrong number of arguments"
    (argExprs', ts') <- liftM unzip $ mapM staticCheck_Expr argExprs
    let m (pos, t1, t2) = assertTypesEqual pos t1 t2
    mapM_ m (zip3 (map getPos_Expr argExprs) argTs ts')
    return (EApp retT funExpr' argExprs', retT)
staticCheck_Expr (EUnaryOp pos op expr) = do
    (expr', t) <- staticCheck_Expr expr
    assertTypesEqual pos t (unaryOpType op)
    let op' = toVoid op
    return (EUnaryOp t op' expr', t)
staticCheck_Expr (EMul pos expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual pos intT t1
    assertTypesEqual pos intT t2
    let op' = toVoid op
    return (EMul intT expr1' op' expr2', intT)
staticCheck_Expr (EAdd pos expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual pos intT t1
    assertTypesEqual pos intT t2
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
staticCheck_Expr (EObjNew pos cls) = do
    classes <- gets classEnv
    assert pos (M.member cls classes) "Class doesn't exist"
    let t = Class () cls
    return (EObjNew t cls, t)
staticCheck_Expr (EArrNew pos t expr) = do
    (expr', t') <- staticCheck_Expr expr
    assertTypesEqual pos intT t'
    let newt = Array () (toUnit t)
    return (EArrNew newt (toVoid t) expr', newt)
staticCheck_Expr (ELambda pos args stmt) = doWithSavedEnv m
    where m = do
            mapM_ (\(Arg pos' t x) -> declareVar pos x (toUnit t)) args
            stmt' <- staticCheck_Stmt stmt
            tOut <- staticCheck_getRetExpr stmt
            let ts = map (\(Arg _ t _) -> toUnit t) args
            let t' = Fun () tOut ts
            let args' = fmap toVoid args
            return (ELambda t' args' stmt', t')

staticCheck_getRetExpr :: Stmt Pos -> CheckM TType
staticCheck_getRetExpr stmt = do
    ts <- staticCheck_RetType stmt
    case ts of
      [] -> return (Void ())
      (x:xs) -> do
                mapM_ (assertTypesEqual Nothing x) xs
                return x

staticCheck_RetType :: Stmt Pos -> CheckM [TType]
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
getPos_Expr (EObjNew pos _) = pos
getPos_Expr (EArrNew pos _ _) = pos
getPos_Expr (ELambda pos _ _) = pos
