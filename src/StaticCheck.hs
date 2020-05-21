module StaticCheck (
    staticCheck
                   ) where

import AbsSPL
import CheckM
import ErrM

import Control.Monad
import Control.Monad.State

import Data.Map as Map

staticCheck :: Program FData -> Err ()
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

tryGetClassName :: TType -> CheckM Ident
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

staticCheck_Program :: Program FData -> CheckM ()
staticCheck_Program (Prog _ topdefs) = do
    mapM_ declareTopDef topdefs
    mapM_ staticCheck_TopDef topdefs

staticCheck_TopDef :: TopDef FData -> CheckM ()
staticCheck_TopDef (FnDef _ t name args block) = doWithSavedEnv m
    where m = do
            modify (\s -> s { rettype = t })
            mapM_ declareArg args
            staticCheck_Stmt (BStmt () block)
staticCheck_TopDef (ClDef _ _ _) = return ()

staticCheck_Stmt :: Stmt FData -> CheckM ()
staticCheck_Stmt (Empty _) = return ()
staticCheck_Stmt (BStmt _ (Bl _ stmts)) =
    doWithSavedEnv (mapM_ staticCheck_Stmt stmts)
staticCheck_Stmt (Decl _ t items) =
    mapM_ (staticCheck_Decl t) items
staticCheck_Stmt (Ass _ e1 e2) = do
    t1 <- staticCheck_LExpr e1
    t2 <- staticCheck_Expr e2
    assertTypesEqual t1 t2
staticCheck_Stmt (Incr _ e) =
    assertTypesEqual intT =<< staticCheck_LExpr e
staticCheck_Stmt (Decr _ e) =
    assertTypesEqual intT =<< staticCheck_LExpr e
staticCheck_Stmt (Ret _ e) = do
    t1 <- getReturnType
    assertTypesEqual t1 =<< staticCheck_Expr e
staticCheck_Stmt (VRet _) =
    assertTypesEqual voidT =<< getReturnType
staticCheck_Stmt (Cond _ e s) = do
    assertTypesEqual boolT =<< staticCheck_Expr e
    staticCheck_Stmt s
staticCheck_Stmt (CondElse _ e s1 s2) = do
    assertTypesEqual boolT =<< staticCheck_Expr e
    staticCheck_Stmt s1
    staticCheck_Stmt s2
staticCheck_Stmt (While _ e s) = do
    assertTypesEqual boolT =<< staticCheck_Expr e
    staticCheck_Stmt s
staticCheck_Stmt (SExp _ e) =
    staticCheck_Expr e >> return ()


staticCheck_Decl :: TType -> Item FData -> CheckM ()
staticCheck_Decl t (NoInit _ name) = declareVar name t
staticCheck_Decl t (Init _ name expr) = do
    assertTypesEqual t =<< staticCheck_Expr expr
    declareVar name t

staticCheck_LExpr :: Expr FData -> CheckM TType
staticCheck_LExpr e@(EVar _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EField _ _ _) = staticCheck_Expr e
staticCheck_LExpr e@(EArrAcc _ _ _) = staticCheck_Expr e
staticCheck_LExpr _ = errorMsg () "Wrong lexpr"


staticCheck_Expr :: Expr FData -> CheckM TType
staticCheck_Expr (EInt _ _) = return intT
staticCheck_Expr (ETrue _) = return boolT
staticCheck_Expr (EFalse _) = return boolT
staticCheck_Expr (ENull _) = return nullT
staticCheck_Expr (EVar _ name) = getVariableType () name
staticCheck_Expr (EField _ expr field) = do
    cls <- tryGetClassName =<< staticCheck_Expr expr
    classes <- gets fields
    assert (Map.member cls classes) "Class doesn't exist"
    assert (Map.member field (classes ! cls)) "Field doesn't exist"
    return ((classes ! cls) ! field)
staticCheck_Expr (EArrAcc _ arrExpr indexExpr) = do
    arrType <- staticCheck_Expr arrExpr
    indexType <- staticCheck_Expr indexExpr
    assert (indexType == intT) "Index is not int"
    tryGetArrayType arrType
staticCheck_Expr (EApp _ funExpr argExprs) = do
    funType <- staticCheck_Expr funExpr
    (retType, argTypes) <- tryGetFunctionType funType
    assert ((length argExprs) == (length argTypes)) "Wrong number of arguments"
    types <- mapM staticCheck_Expr argExprs
    zipWithM_ assertTypesEqual argTypes types
    return retType
staticCheck_Expr (EUnaryOp _ op expr) = do
    t <- staticCheck_Expr expr
    assertTypesEqual t (unaryOpType op)
    return t
staticCheck_Expr (EMul _ expr1 op expr2) =
    staticCheck_BinOp expr1 expr2 (mulOpType op)
staticCheck_Expr (EAdd _ expr1 op expr2) =
    staticCheck_BinOp expr1 expr2 (addOpType op)
staticCheck_Expr (ERel _ e1 (EQU _) e2) = do
    t1 <- staticCheck_Expr e1
    t2 <- staticCheck_Expr e2
    assertTypesEqual t1 t2
    return boolT
staticCheck_Expr (ERel _ e1 (NE _) e2) = do
    t1 <- staticCheck_Expr e1
    t2 <- staticCheck_Expr e2
    assertTypesEqual t1 t2
    return boolT
staticCheck_Expr (ERel _ e1 op e2) = do
    staticCheck_BinOp e1 e2 (relOpType op)
    return boolT
staticCheck_Expr (EAnd _ e1 e2) =
    staticCheck_BinOp e1 e2 boolT
staticCheck_Expr (EOr _ e1 e2) =
    staticCheck_BinOp e1 e2 boolT
staticCheck_Expr (EObjNew _ cls) = do
    classes <- gets fields
    assert (Map.member cls classes) "Class doesn't exist"
    return (Class () cls)
staticCheck_Expr (EArrNew _ t expr) = do
    assertTypesEqual intT =<< staticCheck_Expr expr
    return (Array () t)

staticCheck_BinOp :: Expr FData -> Expr FData -> TType -> CheckM TType
staticCheck_BinOp e1 e2 t = do
    assertTypesEqual t =<< staticCheck_Expr e1
    assertTypesEqual t =<< staticCheck_Expr e2
    return t
