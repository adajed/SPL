module ExprTypeCheck where

import AbsSPL
import ErrM

import CheckM
import Control.Monad ( liftM, liftM2, liftM3, when, (<=<) )
import Control.Monad.Trans.State

import Data.List as List
import Data.Map as Map

intT, boolT, voidT :: Type ()
intT  = (Basic () (Int ()))
boolT = (Basic () (Bool ()))
voidT = (Basic () (Void ()))

toVoid :: Functor f => f a -> f (Type ())
toVoid f = fmap (const voidT) f

typeProgram :: Program FData -> Err (Program (Type ()))
typeProgram program = runCheckM (typedExpr_Program program)

typedExpr_Program :: Program FData -> CheckM (Program (Type ()))
typedExpr_Program (Prog _ topdefs) = do
    topdefsT <- mapM typedExpr_TopDef topdefs
    return (Prog voidT topdefsT)

typedExpr_TopDef :: TopDef FData -> CheckM (TopDef (Type ()))
typedExpr_TopDef (FnDef _ t name args (Bl _ stmts)) = do
    let argsT = List.map toVoid args
    env <- gets varenv
    mapM_ declareArg args
    stmtsT <- mapM typedExpr_Stmt stmts
    modify (\s -> s { varenv = env })
    return (FnDef voidT (toVoid t) name argsT (Bl voidT stmtsT))

declareArg :: Argument FData -> CheckM ()
declareArg (Arg _ t name) = declareVar name (fmap (const ()) t)


typedExpr_Stmt :: Stmt FData -> CheckM (Stmt (Type ()))
typedExpr_Stmt (Empty _) = return (Empty voidT)
typedExpr_Stmt (BStmt _ (Bl _ stmts)) = do
    env <- gets varenv
    stmtsT <- mapM typedExpr_Stmt stmts
    modify (\s -> s { varenv = env } )
    return $ BStmt voidT (Bl voidT stmtsT)
typedExpr_Stmt (Decl _ t decls) = do
    declsT <- mapM (typedExpr_Item t) decls
    return $ Decl voidT (toVoid t) declsT
typedExpr_Stmt (Ass _ expr1 expr2) = do
    liftM2 (Ass voidT) sT1 sT2
        where sT1 = typedExpr_Expr expr1
              sT2 = typedExpr_Expr expr2
typedExpr_Stmt (Incr _ expr) =
    liftM (Incr voidT) (typedExpr_Expr expr)
typedExpr_Stmt (Decr _ expr) = do
    liftM (Decr voidT) (typedExpr_Expr expr)
typedExpr_Stmt (Ret _ expr) = do
    liftM (Ret voidT) (typedExpr_Expr expr)
typedExpr_Stmt (VRet _) = return (VRet voidT)
typedExpr_Stmt (Cond _ expr stmt) =
    liftM2 (Cond voidT) eT sT
        where eT = typedExpr_Expr expr
              sT = typedExpr_Stmt stmt
typedExpr_Stmt (CondElse _ expr stmt1 stmt2) =
    liftM3 (CondElse voidT) eT sT1 sT2
        where eT = typedExpr_Expr expr
              sT1 = typedExpr_Stmt stmt1
              sT2 = typedExpr_Stmt stmt2
typedExpr_Stmt (While _ expr stmt) =
    liftM2 (While voidT) eT sT
        where eT = typedExpr_Expr expr
              sT = typedExpr_Stmt stmt
typedExpr_Stmt (SExp _ expr) =
    liftM (SExp voidT) (typedExpr_Expr expr)

typedExpr_Item :: Type FData -> Item FData -> CheckM (Item (Type ()))
typedExpr_Item t (NoInit _ name) = do
    declareVar name (fmap (const ()) t)
    return (NoInit voidT name)
typedExpr_Item t (Init _ name expr) = do
    declareVar name (fmap (const ()) t)
    liftM (Init voidT name) (typedExpr_Expr expr)

typedExpr_Expr :: Expr FData -> CheckM (Expr (Type ()))
typedExpr_Expr expr = liftM fst (typedExpr expr)

typedExpr :: Expr FData -> CheckM (Expr (Type ()), Type ())
typedExpr (EInt _ n) = return (EInt intT n, intT)
typedExpr (EFalse _) = return (EFalse boolT, boolT)
typedExpr (ETrue _) = return (ETrue boolT, boolT)
typedExpr (EVar pos name) = do
    t <- getVariableType pos name
    return (EVar t name, t)
typedExpr (EArrAcc pos aExpr iExpr) = do
    (iExprT, iT) <- typedExpr iExpr
    (aExprT, aT) <- typedExpr aExpr
    case aT of
      Array _ t -> return (EArrAcc t aExprT iExprT, t)
      _ -> errorMsg pos "Trying to index not an array"
typedExpr (EApp pos fExpr exprs) = do
    exprsT <- mapM (\e -> liftM fst (typedExpr e)) exprs
    (fExprT, fT) <- typedExpr fExpr
    case fT of
      Fun _ t _ -> return (EApp t fExprT exprsT, t)
      _ -> errorMsg pos "Trying to apply not a function"
typedExpr (EUnaryOp _ op expr) = do
    (exprT, t) <- typedExpr expr
    return (EUnaryOp t (toVoid op) exprT, t)
typedExpr (EMul pos expr1 op expr2) = do
    (exprT1, t1) <- typedExpr expr1
    (exprT2, t2) <- typedExpr expr2
    when (t1 /= t2) (errorMsg pos "Expr types don't match")
    return (EMul t1 exprT1 (toVoid op) exprT2, t1)
typedExpr (EAdd pos expr1 op expr2) = do
    (exprT1, t1) <- typedExpr expr1
    (exprT2, t2) <- typedExpr expr2
    when (t1 /= t2) (errorMsg pos "Expr types don't match")
    return (EAdd t1 exprT1 (toVoid op) exprT2, t1)
typedExpr (ERel pos expr1 op expr2) = do
    (exprT1, t1) <- typedExpr expr1
    (exprT2, t2) <- typedExpr expr2
    when (t1 /= t2) (errorMsg pos "Expr types don't match")
    return (ERel t1 exprT1 (toVoid op) exprT2, t1)
typedExpr (EAnd pos expr1 expr2) = do
    (exprT1, t1) <- typedExpr expr1
    (exprT2, t2) <- typedExpr expr2
    when (t1 /= t2) (errorMsg pos "Expr types don't match")
    return (EAnd t1 exprT1 exprT2, t1)
typedExpr (EOr pos expr1 expr2) = do
    (exprT1, t1) <- typedExpr expr1
    (exprT2, t2) <- typedExpr expr2
    when (t1 /= t2) (errorMsg pos "Expr types don't match")
    return (EOr t1 exprT1 exprT2, t1)
typedExpr (EArrNew _ argT expr) = do
    (exprT, t) <- typedExpr expr
    when (t /= intT) (errorMsg () "Size is not int")
    let aT = fmap (const ()) argT
    return (EArrNew aT (toVoid argT) exprT, aT)
