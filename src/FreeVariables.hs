module FreeVariables where

import AbsSPL
import Token ( VIdent(..) )
import Type

import qualified Data.Set as S

import Control.Monad.Trans.State
import Control.Monad.Identity

data SState = SState { usedVars :: S.Set VIdent
                     , envName :: VIdent
                     , envType :: Type T
                     }

type GenM a = StateT SState Identity a

getFreeVars :: Stmt T -> S.Set VIdent
getFreeVars stmt = runIdentity (evalStateT m initState)
    where m = calcFreeVars stmt
          initState = SState { usedVars = S.empty
                             , envName = VIdent ""
                             , envType = Void (Void ())
                             }

addVar :: VIdent -> GenM ()
addVar x = modify (\s -> s { usedVars = f (usedVars s) })
    where f = S.insert x

removeVar :: VIdent -> GenM ()
removeVar x = modify (\s -> s { usedVars = f (usedVars s) })
    where f = S.delete x

checkVar :: VIdent -> GenM Bool
checkVar x = liftM (S.member x) $ gets usedVars

calcFreeVars :: Stmt T -> GenM (S.Set VIdent)
calcFreeVars (Empty _) = return S.empty
calcFreeVars (BStmt _ (Bl _ stmts)) = do
    set <- gets usedVars
    fs <- mapM calcFreeVars stmts
    modify (\s -> s { usedVars = set })
    return (S.unions fs)
calcFreeVars (Decl _ _ items) =
    liftM S.unions $ mapM calcFreeVars_Item items
calcFreeVars (Ass _ expr1 expr2) = do
    s1 <- calcFreeVars_Expr expr1
    s2 <- calcFreeVars_Expr expr2
    return (S.union s1 s2)
calcFreeVars (Incr _ expr) = calcFreeVars_Expr expr
calcFreeVars (Decr _ expr) = calcFreeVars_Expr expr
calcFreeVars (Ret _ expr) = calcFreeVars_Expr expr
calcFreeVars (Cond _ expr stmt) = do
    s1 <- calcFreeVars_Expr expr
    s2 <- calcFreeVars stmt
    return (S.union s1 s2)
calcFreeVars (CondElse _ expr stmt1 stmt2) = do
    s1 <- calcFreeVars_Expr expr
    s2 <- calcFreeVars stmt1
    s3 <- calcFreeVars stmt2
    return (S.unions [s1, s2, s3])
calcFreeVars (While _ expr stmt) = do
    s1 <- calcFreeVars_Expr expr
    s2 <- calcFreeVars stmt
    return (S.union s1 s2)
calcFreeVars (SExp _ expr) = calcFreeVars_Expr expr

calcFreeVars_Item :: Item T -> GenM (S.Set VIdent)
calcFreeVars_Item  (NoInit _ name) = do
    addVar name
    return S.empty
calcFreeVars_Item (Init _ name expr) = do
    f <- calcFreeVars_Expr expr
    addVar name
    return f

calcFreeVars_Expr :: Expr T -> GenM (S.Set VIdent)
calcFreeVars_Expr (ENull _) = return S.empty
calcFreeVars_Expr (EInt _ _) = return S.empty
calcFreeVars_Expr (ETrue _) = return S.empty
calcFreeVars_Expr (EFalse _) = return S.empty
calcFreeVars_Expr (EVar _ name) = do
    b <- checkVar name
    let set = if b then S.empty
                   else S.singleton name
    return set
calcFreeVars_Expr (EField _ expr _) =
    calcFreeVars_Expr expr
calcFreeVars_Expr (EArrAcc _ expr1 expr2) = do
    s1 <- calcFreeVars_Expr expr1
    s2 <- calcFreeVars_Expr expr2
    return (S.union s1 s2)
calcFreeVars_Expr (EApp _ expr1 exprs) = do
    s <- calcFreeVars_Expr expr1
    sx <- mapM calcFreeVars_Expr exprs
    return (S.unions (s:sx))
calcFreeVars_Expr (EUnaryOp _ _ expr) =
    calcFreeVars_Expr expr
calcFreeVars_Expr (EBinOp _ expr1 _ expr2) = do
    s1 <- calcFreeVars_Expr expr1
    s2 <- calcFreeVars_Expr expr2
    return (S.union s1 s2)
calcFreeVars_Expr (EObjNew _ _ exprs) = liftM S.unions $ mapM calcFreeVars_Expr exprs
calcFreeVars_Expr (EArrNew _ _ expr) =
    calcFreeVars_Expr expr
calcFreeVars_Expr (ELambda _ args stmt) = do
    set <- gets usedVars
    mapM_ (\(Arg _ _ name) -> addVar name) args
    s <- calcFreeVars stmt
    modify (\s -> s { usedVars = set })
    return s


substitute :: Type T -> VIdent -> S.Set VIdent -> Stmt T -> Stmt T
substitute ty name vars stmt = runIdentity (evalStateT m initState)
    where m = substitute_Stmt stmt
          initState = SState { usedVars = vars
                             , envType = ty
                             , envName = name
                             }


substitute_Stmt :: Stmt T -> GenM (Stmt T)
substitute_Stmt (BStmt t1 (Bl t2 stmts)) = do
    set <- gets usedVars
    stmts' <- mapM substitute_Stmt stmts
    modify (\s -> s { usedVars = set })
    return (BStmt t1 (Bl t2 stmts'))
substitute_Stmt (Decl t ty items) = do
    items' <- mapM substitute_Item items
    return (Decl t ty items')
substitute_Stmt (Incr t e) = do
    e' <- substitute_Expr e
    return (Incr t e')
substitute_Stmt (Decr t e) = do
    e' <- substitute_Expr e
    return (Decr t e')
substitute_Stmt (Ret t e) = do
    e' <- substitute_Expr e
    return (Ret t e')
substitute_Stmt (Cond t e s) = do
    e' <- substitute_Expr e
    s' <- substitute_Stmt s
    return (Cond t e' s')
substitute_Stmt (CondElse t e s1 s2) = do
    e' <- substitute_Expr e
    s1' <- substitute_Stmt s1
    s2' <- substitute_Stmt s2
    return (CondElse t e' s1' s2')
substitute_Stmt (While t e s) = do
    e' <- substitute_Expr e
    s' <- substitute_Stmt s
    return (While t e' s')
substitute_Stmt (SExp t e) = do
    e' <- substitute_Expr e
    return (SExp t e')
substitute_Stmt stmt = return stmt

substitute_Item :: Item T -> GenM (Item T)
substitute_Item (NoInit t name) = do
    removeVar name
    return (NoInit t name)
substitute_Item (Init t name e) = do
    e' <- substitute_Expr e
    removeVar name
    return (Init t name e')

substitute_Expr :: Expr T -> GenM (Expr T)
substitute_Expr (EVar t name) = do
    b <- checkVar name
    ty <- gets envType
    env <- gets envName
    if b then return (EField t (EVar (fmap (const ()) ty) env) name)
         else return (EVar t name)
substitute_Expr (EArrAcc t e1 e2) = do
    e1' <- substitute_Expr e1
    e2' <- substitute_Expr e2
    return (EArrAcc t e1' e2')
substitute_Expr (EApp t e ex) = do
    e' <- substitute_Expr e
    ex' <- mapM substitute_Expr ex
    return (EApp t e' ex')
substitute_Expr (EUnaryOp t op e) = do
    e' <- substitute_Expr e
    return (EUnaryOp t op e)
substitute_Expr (EBinOp t e1 op e2) = do
    e1' <- substitute_Expr e1
    e2' <- substitute_Expr e2
    return (EBinOp t e1' op e2')
substitute_Expr (EArrNew t ty e) = do
    e' <- substitute_Expr e
    return (EArrNew t ty e')
substitute_Expr (ELambda t args stmt) = do
    set <- gets usedVars
    mapM_ (\(Arg _ _ x) -> removeVar x) args
    stmt' <- substitute_Stmt stmt
    modify (\s -> s { usedVars = set })
    return (ELambda t args stmt')
substitute_Expr expr = return expr

