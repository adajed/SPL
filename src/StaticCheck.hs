module StaticCheck (
    staticCheck
                   ) where

import AbsSPL
import CheckM
import ErrM

import Control.Monad
import Control.Monad.State

import Data.Map as Map

staticCheck :: Program FData -> Err (Program TType)
staticCheck program = runCheckM (staticCheck_Program program)

assert :: Bool -> String -> CheckM ()
assert b msg = unless b (errorMsg () msg)

assertTypesEqual :: TType -> TType -> CheckM ()
assertTypesEqual (Class _ _) (Null _) = return ()
assertTypesEqual (Null _) (Class _ _) = return ()
assertTypesEqual t1 t2 = assert (t1 == t2) msg
    where msg = "Types don't match: " ++ show t1 ++ " != " ++ show t2

tryGetArrayType :: TType -> CheckM TType
tryGetArrayType (Array _ t) = return t
tryGetArrayType _ = errorMsg () "Type is not array"

tryGetFunctionType :: TType -> CheckM (TType, [TType])
tryGetFunctionType (Fun _ retType argTypes) = return (retType, argTypes)
tryGetFunctionType  _ = errorMsg () "Type is not function"

tryGetClassName :: TType -> CheckM CIdent
tryGetClassName (Class _ cls) = return cls
tryGetClassName _ = errorMsg () "Type is not a class"

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

toVoid :: Functor f => f a -> f TType
toVoid f = fmap (const voidT) f

staticCheck_Program :: Program FData -> CheckM (Program TType)
staticCheck_Program (Prog _ topdefs) = do
    mapM_ declareTopDef topdefs
    topdefs' <- mapM staticCheck_TopDef topdefs
    return (Prog voidT topdefs')

staticCheck_TopDef :: TopDef FData -> CheckM (TopDef TType)
staticCheck_TopDef (FnDef _ t name args (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            modify (\s -> s { rettype = t })
            mapM_ declareArg args
            stmts' <- mapM staticCheck_Stmt stmts
            let t' = toVoid t
            let args' = fmap toVoid args
            return (FnDef voidT t' name args' (Bl voidT stmts'))
staticCheck_TopDef t@(ClDef _ _ _) = return (toVoid t)

staticCheck_Stmt :: Stmt FData -> CheckM (Stmt TType)
staticCheck_Stmt (Empty _) = return (Empty voidT)
staticCheck_Stmt (BStmt _ (Bl _ stmts)) = doWithSavedEnv m
    where m = do
            stmts' <- mapM staticCheck_Stmt stmts
            return (BStmt voidT (Bl voidT stmts'))
staticCheck_Stmt (Decl _ t items) = do
    items' <- mapM (staticCheck_Decl t) items
    let t' = toVoid t
    return (Decl voidT t' items')
staticCheck_Stmt (Ass _ e1 e2) = do
    (e1', t1) <- staticCheck_LExpr e1
    (e2', t2) <- staticCheck_Expr e2
    assertTypesEqual t1 t2
    return (Ass voidT e1' e2')
staticCheck_Stmt (Incr _ e) = do
    (e', t) <- staticCheck_LExpr e
    assertTypesEqual intT t
    return (Incr voidT e')
staticCheck_Stmt (Decr _ e) = do
    (e', t) <- staticCheck_LExpr e
    assertTypesEqual intT t
    return (Decr voidT e')
staticCheck_Stmt (Ret _ e) = do
    (e', t) <- staticCheck_Expr e
    t1 <- getReturnType
    assertTypesEqual t1 t
    return (Ret voidT e')
staticCheck_Stmt (VRet _) = do
    assertTypesEqual voidT =<< getReturnType
    return (VRet voidT)
staticCheck_Stmt (Cond _ e s) = do
    (e', t) <- staticCheck_Expr e
    assertTypesEqual boolT t
    s' <- staticCheck_Stmt s
    return (Cond voidT e' s')
staticCheck_Stmt (CondElse _ e s1 s2) = do
    (e', t) <- staticCheck_Expr e
    assertTypesEqual boolT t
    s1' <- staticCheck_Stmt s1
    s2' <- staticCheck_Stmt s2
    return (CondElse voidT e' s1' s2')
staticCheck_Stmt (While _ e s) = do
    (e', t) <- staticCheck_Expr e
    assertTypesEqual boolT t
    s' <- staticCheck_Stmt s
    return (While voidT e' s')
staticCheck_Stmt (SExp _ e) = do
    (e', _) <- staticCheck_Expr e
    return (SExp voidT e')


staticCheck_Decl :: TType -> Item FData -> CheckM (Item TType)
staticCheck_Decl t (NoInit _ name) = do
    declareVar name t
    return (NoInit voidT name)
staticCheck_Decl t (Init _ name expr) = do
    (expr', t') <- staticCheck_Expr expr
    assertTypesEqual t t'
    declareVar name t
    return (Init voidT name expr')

staticCheck_LExpr :: Expr FData -> CheckM (Expr TType, TType)
staticCheck_LExpr e@(EVar _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EField _ _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EArrAcc _ _ _) = staticCheck_Expr e
staticCheck_LExpr _ = errorMsg () "Wrong lexpr"


staticCheck_Expr :: Expr FData -> CheckM (Expr TType, TType)
staticCheck_Expr (EInt _ n) = return (EInt intT n, intT)
staticCheck_Expr (ETrue _) = return (ETrue boolT, boolT)
staticCheck_Expr (EFalse _) = return (EFalse boolT, boolT)
staticCheck_Expr (ENull _) = return (ENull nullT, nullT)
staticCheck_Expr (EVar _ name) = do
    t <- getVariableType () name
    return (EVar t name, t)
staticCheck_Expr (EField _ expr field) = do
    (expr', t') <- staticCheck_Expr expr
    cls <- tryGetClassName t'
    classes <- gets fields
    assert (Map.member cls classes) "Class doesn't exist"
    assert (Map.member field (classes ! cls)) "Field doesn't exist"
    let t = (classes ! cls) ! field
    return (EField t expr' field, t)
staticCheck_Expr (EArrAcc _ arrExpr indexExpr) = do
    (arrExpr', arrT) <- staticCheck_Expr arrExpr
    (indexExpr', indexT) <- staticCheck_Expr indexExpr
    assert (indexT == intT) "Index is not int"
    t <- tryGetArrayType arrT
    return (EArrAcc t arrExpr' indexExpr', t)
staticCheck_Expr (EApp _ funExpr argExprs) = do
    (funExpr', funT) <- staticCheck_Expr funExpr
    (retT, argTs) <- tryGetFunctionType funT
    assert ((length argExprs) == (length argTs)) "Wrong number of arguments"
    (argExprs', ts') <- liftM unzip $ mapM staticCheck_Expr argExprs
    zipWithM_ assertTypesEqual argTs ts'
    return (EApp retT funExpr' argExprs', retT)
staticCheck_Expr (EUnaryOp _ op expr) = do
    (expr', t) <- staticCheck_Expr expr
    assertTypesEqual t (unaryOpType op)
    let op' = toVoid op
    return (EUnaryOp t op' expr', t)
staticCheck_Expr (EMul _ expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual intT t1
    assertTypesEqual intT t2
    let op' = toVoid op
    return (EMul intT expr1' op' expr2', intT)
staticCheck_Expr (EAdd _ expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual intT t1
    assertTypesEqual intT t2
    let op' = toVoid op
    return (EAdd intT expr1' op' expr2', intT)
staticCheck_Expr (ERel _ expr1 (EQU _) expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual t1 t2
    let op' = EQU voidT
    return (ERel boolT expr1' op' expr2', boolT)
staticCheck_Expr (ERel _ expr1 (NE _) expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual t1 t2
    let op' = NE voidT
    return (ERel boolT expr1' op' expr2', boolT)
staticCheck_Expr (ERel _ expr1 op expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual intT t1
    assertTypesEqual intT t2
    let op' = toVoid op
    return (ERel boolT expr1' op' expr2', boolT)
staticCheck_Expr (EAnd _ expr1 expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual boolT t1
    assertTypesEqual boolT t2
    return (EAnd boolT expr1' expr2', boolT)
staticCheck_Expr (EOr _ expr1 expr2) = do
    (expr1', t1) <- staticCheck_Expr expr1
    (expr2', t2) <- staticCheck_Expr expr2
    assertTypesEqual boolT t1
    assertTypesEqual boolT t2
    return (EOr boolT expr1' expr2', boolT)
staticCheck_Expr (EObjNew _ cls) = do
    classes <- gets fields
    assert (Map.member cls classes) "Class doesn't exist"
    let t = Class () cls
    return (EObjNew t cls, t)
staticCheck_Expr (EArrNew _ t expr) = do
    (expr', t') <- staticCheck_Expr expr
    assertTypesEqual intT t'
    let newt = Array () t
    return (EArrNew newt (toVoid t) expr', newt)
staticCheck_Expr (ELambda _ args stmt) = doWithSavedEnv m
    where m = do
            mapM_ (\(Arg _ t x) -> declareVar x t) args
            stmt' <- staticCheck_Stmt stmt
            tOut <- staticCheck_getRetExpr stmt
            let ts = Prelude.map (\(Arg _ t _) -> t) args
            let t' = Fun () tOut ts
            let args' = fmap toVoid args
            return (ELambda t' args' stmt', t')

staticCheck_getRetExpr :: Stmt FData -> CheckM TType
staticCheck_getRetExpr stmt = do
    ts <- staticCheck_RetType stmt
    case ts of
      [] -> return (Void ())
      (x:xs) -> do
                mapM_ (assertTypesEqual x) xs
                return x

staticCheck_RetType :: Stmt FData -> CheckM [TType]
staticCheck_RetType (BStmt _ (Bl _ stmts)) = do
    liftM concat $ mapM staticCheck_RetType stmts
staticCheck_RetType (Decl _ t items) = do
    mapM_ (staticCheck_Decl t) items
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

