module GenerateIR where

import AbsSPL
import ErrM
import IR

import Data.Map as Map

import Control.Monad ( liftM, unless, void )
import Control.Monad.Trans ( lift )

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

data SState = SState { tempCounter :: Int
                     , levelCounter :: Int
                     , labelCounter :: Int
                     , varenv :: Map Ident Ident
                     , output :: Map Ident [IR]
                     , currentOutput :: [IR]
                     }

type GenerateIR a = StateT SState Err a

type T = Type ()

initialVarEnv :: Map Ident Ident
initialVarEnv = Map.fromList [(Ident "printInt", Ident "printInt")]

initialSState :: SState
initialSState = SState { tempCounter  = 0
                       , levelCounter = 0
                       , labelCounter = 0
                       , varenv = initialVarEnv
                       , output = Map.empty
                       , currentOutput = [] }

runGenerateIR :: Program T -> Err (Map Ident [IR])
runGenerateIR program = liftM output $ execStateT m initialSState
    where m = generateIR_Program program

getsSState :: (SState -> a) -> GenerateIR a
getsSState f = gets f

modifySState :: (SState -> SState) -> GenerateIR ()
modifySState f = modify f

emitIR :: IR -> GenerateIR ()
emitIR ir = modifySState (\s -> s { currentOutput = ir:(currentOutput s) } )

emitIR_ToTemp :: (Var -> IR) -> GenerateIR Var
emitIR_ToTemp f = do
    t <- getFreshTemp
    emitIR (f t)
    return t

getFreshTemp :: GenerateIR Var
getFreshTemp = do
    i <- getsSState tempCounter
    modifySState (\s -> s { tempCounter = i + 1 })
    return (VarT i)

declareVar :: Ident -> GenerateIR Var
declareVar (Ident name) = do
    level <- getsSState levelCounter
    let varName = Ident $ name ++ "_" ++ show level
    modifySState (\s -> s { varenv = Map.insert (Ident name) varName (varenv s) } )
    return $ VarN varName

declareFunction :: TopDef T -> GenerateIR ()
declareFunction (FnDef _ _ name _ _) = void $ declareVar name

getVar :: Ident -> GenerateIR Var
getVar name = do
    env <- getsSState varenv
    unless (Map.member name env) (fail ("Variable not declared: " ++ show name))
    return $ VarN $ env ! name

getFreshLabel :: GenerateIR Ident
getFreshLabel = do
    i <- getsSState labelCounter
    modifySState (\s -> s { labelCounter = i + 1 } )
    return $ Ident $ ".L" ++ show i

defaultValue :: Type a -> Value
defaultValue (Int _) = VInt 0
defaultValue (Bool _) = VBool False
defaultValue (Void _) = VVoid

generateIR_Program :: Program T -> GenerateIR ()
generateIR_Program (Prog t topdefs) = do
    mapM_ declareFunction topdefs
    mapM_ generateIR_TopDef topdefs

generateIR_TopDef :: TopDef T -> GenerateIR ()
generateIR_TopDef (FnDef _ t name args (Bl _ stmts)) = do
    modifySState (\s -> s { currentOutput = [] } )
    env <- getsSState varenv
    emitIR (IR_Label name)
    mapM_ declareArg args
    modifySState (\s -> s { levelCounter = (levelCounter s) + 1 } )
    mapM_ generateIR_Stmt stmts
    modifySState (\s -> s { levelCounter = (levelCounter s) - 1
                          , varenv = env
                          , output = Map.insert name ((reverse . currentOutput) s) (output s) } )

declareArg :: Argument T -> GenerateIR ()
declareArg (Arg _ _ name) = do
    var <- declareVar name
    emitIR (IR_Argument var)

generateIR_Stmt :: Stmt T -> GenerateIR ()
generateIR_Stmt (Empty _) = return ()
generateIR_Stmt (BStmt _ (Bl _ stmts)) = do
    env <- getsSState varenv
    modifySState (\s -> s { levelCounter = (levelCounter s) + 1 } )
    mapM_ generateIR_Stmt stmts
    modifySState (\s -> s { levelCounter = (levelCounter s) - 1 } )
    modifySState (\s -> s { varenv = env } )
generateIR_Stmt (Decl _ t items) = do
    mapM_ (generateIR_Decl t) items
generateIR_Stmt (Ass _ expr1 expr2) = do
    fAss <- generateIR_LExpr expr1
    value <- liftM VVar $ generateIR_Expr expr2
    fAss value
    -- emitIR (IR_Ass var value)
generateIR_Stmt (Incr _ expr) = do
    value <- liftM VVar $ generateIR_Expr expr
    f <- generateIR_LExpr expr
    temp <- emitIR_ToTemp (\t -> IR_BinOp (BOpInt IAdd) t value (VInt 1))
    f (VVar temp)
generateIR_Stmt (Decr _ expr) = do
    value <- liftM VVar $ generateIR_Expr expr
    f <- generateIR_LExpr expr
    temp <- emitIR_ToTemp (\t -> IR_BinOp (BOpInt ISub) t value (VInt 1))
    f (VVar temp)
generateIR_Stmt (Ret _ expr) = do
    value <- liftM VVar $ generateIR_Expr expr
    emitIR (IR_Return value)
generateIR_Stmt (VRet _) = do
    emitIR (IR_Return VVoid)
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

generateIR_Decl :: Type a -> Item T -> GenerateIR ()
generateIR_Decl t (NoInit _ name) = do
    var <- declareVar name
    emitIR (IR_Ass var (defaultValue t))
generateIR_Decl t (Init _ name expr) = do
    var <- declareVar name
    temp <- liftM VVar $ generateIR_Expr expr
    emitIR (IR_Ass var temp)


relop :: AbsSPL.RelOp a -> IR.RelOp
relop op = case op of
              AbsSPL.LTH _ -> IR.LTH
              AbsSPL.LE _  -> IR.LEQ
              AbsSPL.GTH _ -> IR.GTH
              AbsSPL.GE _  -> IR.GEQ
              AbsSPL.EQU _ -> IR.EQU
              AbsSPL.NE _  -> IR.NEQ


generateIR_Expr :: Expr T -> GenerateIR Var
generateIR_Expr (ETypedExpr _ _ expr) = generateIR_Expr expr
generateIR_Expr (EInt _ n) = emitIR_ToTemp (\t -> IR_Ass t (VInt (fromInteger n)))
generateIR_Expr (ETrue _) = emitIR_ToTemp (\t -> IR_Ass t (VBool True))
generateIR_Expr (EFalse _) = emitIR_ToTemp (\t -> IR_Ass t (VBool False))
generateIR_Expr (EVar _ name) = do
    var <- getVar name
    emitIR_ToTemp (\t -> IR_Ass t (VVar var))
generateIR_Expr expr@(EArrAcc type_ arr index) = do
    arrV <- generateIR_Expr arr
    ind <- liftM VVar $ generateIR_Expr index
    let size = sizeOf type_
    v' <- liftM VVar $ emitIR_ToTemp (\t -> IR_BinOp (BOpInt IMul) t ind (VInt size))
    loc <- emitIR_ToTemp (\t -> IR_BinOp (BOpInt IAdd) t (VVar arrV) v')
    emitIR_ToTemp (\t -> IR_MemRead t (VVar loc))
generateIR_Expr (EApp _ (EVar _ fName) args) = do
    let numberOfArgs = length args
    let putParam expr = generateIR_Expr expr >>= emitIR . IR_Param . VVar
    mapM_ putParam args
    emitIR_ToTemp (\t -> IR_Call t (VLabel fName) numberOfArgs)
generateIR_Expr (EUnaryOp _ op expr) = generateIR_UnOp op' expr
    where op' = case op of
                  Neg _    -> UOpInt INeg
                  Not _    -> UOpBool BNot
                  BitNot _ -> UOpInt INot
generateIR_Expr (EMul _ expr1 op expr2) = generateIR_BinOp op' expr1 expr2
    where op' = case op of
                  Times _  -> BOpInt IMul
                  Div _    -> BOpInt IDiv
                  Mod _    -> BOpInt IMod
                  LShift _ -> BOpInt ILshift
                  RShift _ -> BOpInt IRshift
                  BitAnd _ -> BOpInt IBitAnd
                  BitOr _  -> BOpInt IBitOr
                  BitXor _ -> BOpInt IBitXor
generateIR_Expr (EAdd _ expr1 op expr2) = generateIR_BinOp op' expr1 expr2
    where op' = case op of
                  Plus _  -> BOpInt IAdd
                  Minus _ -> BOpInt ISub
generateIR_Expr (ERel _ expr1 op expr2) = generateIR_BinOp op' expr1 expr2
    where op' = BOpRel (relop op)
generateIR_Expr (EAnd _ expr1 expr2) = generateIR_BinOp (BOpBool BAnd) expr1 expr2
generateIR_Expr (EOr _ expr1 expr2) = generateIR_BinOp (BOpBool BOr) expr1 expr2
generateIR_Expr (EArrNew tt t expr) =
    generateIR_Expr (EApp tt (EVar tt (Ident "malloc")) [size])
        where size = EMul tt expr (Times tt) (EInt (Int ()) (fromIntegral (sizeOf t)))

generateIR_UnOp :: UnOp -> Expr T -> GenerateIR Var
generateIR_UnOp op expr = do
    v <- liftM VVar $ generateIR_Expr expr
    emitIR_ToTemp (\t -> IR_UnOp op t v)

generateIR_BinOp :: BinOp -> Expr T -> Expr T -> GenerateIR Var
generateIR_BinOp op expr1 expr2 = do
    v1 <- liftM VVar $ generateIR_Expr expr1
    v2 <- liftM VVar $ generateIR_Expr expr2
    emitIR_ToTemp (\t -> IR_BinOp op t v1 v2)

generateIR_LExpr :: Expr T -> GenerateIR (Value -> GenerateIR ())
generateIR_LExpr (EVar _ name) = do
    x <- getVar name
    return (\v -> emitIR (IR_Ass x v))
generateIR_LExpr (EArrAcc type_ arr expr) = do
    x <- generateIR_Expr arr
    ind <- liftM VVar $ generateIR_Expr expr
    let size = sizeOf type_
    v' <- liftM VVar $ emitIR_ToTemp (\t -> IR_BinOp (BOpInt IMul) t ind (VInt size))
    loc <- emitIR_ToTemp (\t -> IR_BinOp (BOpInt IAdd) t (VVar x) v')
    return (\v -> emitIR (IR_MemSave (VVar loc) v))

generateIR_JumpExpr :: Expr T -> Ident -> Ident -> GenerateIR ()
generateIR_JumpExpr (ERel _ e1 op e2) lTrue lFalse = do
    v1 <- liftM VVar $ generateIR_Expr e1
    v2 <- liftM VVar $ generateIR_Expr e2
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

sizeOf :: Type a -> Int
sizeOf (Int _) = 4
sizeOf (Bool _) = 1
sizeOf (Void _) = 0
sizeOf (Array _ _) = 8
sizeOf (Fun _ _ _) = 8
