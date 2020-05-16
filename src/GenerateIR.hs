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
    mapM_ declareArg args
    modifySState (\s -> s { levelCounter = (levelCounter s) + 1 } )
    emitIR (IR_Label name)
    mapM_ generateIR_Stmt stmts
    modifySState (\s -> s { levelCounter = (levelCounter s) - 1
                          , varenv = env
                          , output = Map.insert name ((reverse . currentOutput) s) (output s) } )

declareArg :: Argument T -> GenerateIR ()
declareArg (Arg _ _ name) = void $ declareVar name

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
    var <- generateIR_LExpr expr1
    value <- liftM VVar $ generateIR_Expr expr2
    emitIR (IR_Ass var value)
generateIR_Stmt (Incr _ expr) = do
    value <- liftM VVar $ generateIR_Expr expr
    var <- generateIR_LExpr expr
    temp <- emitIR_ToTemp (\t -> IR_IBinOp IAdd t value (VInt 1))
    emitIR (IR_Ass var (VVar temp))
generateIR_Stmt (Decr _ expr) = do
    value <- liftM VVar $ generateIR_Expr expr
    var <- generateIR_LExpr expr
    temp <- emitIR_ToTemp (\t -> IR_IBinOp ISub t value (VInt 1))
    emitIR (IR_Ass var (VVar temp))
generateIR_Stmt (Ret _ expr) = do
    value <- liftM VVar $ generateIR_Expr expr
    emitIR (IR_Return value)
generateIR_Stmt (VRet _) = do
    emitIR (IR_Return VVoid)
generateIR_Stmt (Cond _ expr stmt) = do
    value <- liftM VVar $ generateIR_Expr expr
    value <- liftM VVar $ emitIR_ToTemp (\t -> IR_BUnOp BNot t value)
    label <- getFreshLabel
    emitIR (IR_CondJump value label)
    generateIR_Stmt stmt
    emitIR (IR_Label label)
generateIR_Stmt (CondElse _ expr stmt1 stmt2) = do
    value <- liftM VVar $ generateIR_Expr expr
    label1 <- getFreshLabel
    label2 <- getFreshLabel
    emitIR (IR_CondJump value label1)
    generateIR_Stmt stmt2
    emitIR (IR_Jump label2)
    emitIR (IR_Label label1)
    generateIR_Stmt stmt1
    emitIR (IR_Label label2)
generateIR_Stmt (While _ expr stmt) = do
    label1 <- getFreshLabel
    label2 <- getFreshLabel
    emitIR (IR_Jump label2)
    emitIR (IR_Label label1)
    generateIR_Stmt stmt
    emitIR (IR_Label label2)
    value <- liftM VVar $ generateIR_Expr expr
    emitIR (IR_CondJump value label1)
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


generateIR_Expr :: Expr T -> GenerateIR Var
generateIR_Expr (ETypedExpr _ _ expr) = generateIR_Expr expr
generateIR_Expr (EInt _ n) = emitIR_ToTemp (\t -> IR_Ass t (VInt n))
generateIR_Expr (ETrue _) = emitIR_ToTemp (\t -> IR_Ass t (VBool True))
generateIR_Expr (EFalse _) = emitIR_ToTemp (\t -> IR_Ass t (VBool False))
generateIR_Expr (EVar _ name) = do
    var <- getVar name
    emitIR_ToTemp (\t -> IR_Ass t (VVar var))
-- generateIR_Expr expr@(EArrAcc type_ arr index) = do
--     var <- generateIR_LExpr expr
--     emitIR_ToTemp (\t -> IR_Memory t (VVar var))
generateIR_Expr (EApp _ (EVar _ fName) args) = do
    let numberOfArgs = length args
    let putParam expr = generateIR_Expr expr >>= emitIR . IR_Param . VVar
    mapM_ putParam args
    emitIR_ToTemp (\t -> IR_Call t (VLabel fName) numberOfArgs)
generateIR_Expr (EUnaryOp _ op expr) = do
    v <- liftM VVar $ generateIR_Expr expr
    case op of
      Neg _ ->    emitIR_ToTemp (\t -> IR_IUnOp INeg t v)
      Not _ ->    emitIR_ToTemp (\t -> IR_BUnOp BNot t v)
      BitNot _ -> emitIR_ToTemp (\t -> IR_IUnOp INot t v)
generateIR_Expr (EMul _ expr1 op expr2) = do
    v1 <- liftM VVar $ generateIR_Expr expr1
    v2 <- liftM VVar $ generateIR_Expr expr2
    case op of
      Times _  -> emitIR_ToTemp (\t -> IR_IBinOp IMul t v1 v2)
      Div _    -> emitIR_ToTemp (\t -> IR_IBinOp IDiv t v1 v2)
      Mod _    -> emitIR_ToTemp (\t -> IR_IBinOp IMod t v1 v2)
      LShift _ -> emitIR_ToTemp (\t -> IR_IBinOp ILshift t v1 v2)
      RShift _ -> emitIR_ToTemp (\t -> IR_IBinOp IRshift t v1 v2)
      BitAnd _ -> emitIR_ToTemp (\t -> IR_IBinOp IAnd t v1 v2)
      BitOr _  -> emitIR_ToTemp (\t -> IR_IBinOp IOr t v1 v2)
      BitXor _ -> emitIR_ToTemp (\t -> IR_IBinOp IXor t v1 v2)
generateIR_Expr (EAdd _ expr1 op expr2) = do
    v1 <- liftM VVar $ generateIR_Expr expr1
    v2 <- liftM VVar $ generateIR_Expr expr2
    case op of
      Plus _ ->  emitIR_ToTemp (\t -> IR_IBinOp IAdd t v1 v2)
      Minus _ -> emitIR_ToTemp (\t -> IR_IBinOp ISub t v1 v2)
generateIR_Expr (ERel _ expr1 op expr2) = do
    v1 <- liftM VVar $ generateIR_Expr expr1
    v2 <- liftM VVar $ generateIR_Expr expr2
    case op of
      LTH _ -> emitIR_ToTemp (\t -> IR_IRelOp ILT t v1 v2)
      LE _  -> emitIR_ToTemp (\t -> IR_IRelOp ILE t v1 v2)
      GTH _ -> emitIR_ToTemp (\t -> IR_IRelOp IGT t v1 v2)
      GE _  -> emitIR_ToTemp (\t -> IR_IRelOp IGE t v1 v2)
      EQU _ -> emitIR_ToTemp (\t -> IR_IRelOp IEQ t v1 v2)
      NE _  -> emitIR_ToTemp (\t -> IR_IRelOp INEQ t v1 v2)
generateIR_Expr (EAnd _ expr1 expr2) = do
    v1 <- liftM VVar $ generateIR_Expr expr1
    v2 <- liftM VVar $ generateIR_Expr expr2
    emitIR_ToTemp (\t -> IR_BBinOp BAnd t v1 v2)
generateIR_Expr (EOr _ expr1 expr2) = do
    v1 <- liftM VVar $ generateIR_Expr expr1
    v2 <- liftM VVar $ generateIR_Expr expr2
    emitIR_ToTemp (\t -> IR_BBinOp BOr t v1 v2)
-- generateIR_Expr (EArrNew tt t expr) =
--     generateIR_Expr (EApp tt (EVar tt (Ident "malloc")) [size])
--         where size = EMul tt expr (Times tt) (EInt (Basic () (Int ())) (sizeOf t))

generateIR_LExpr :: Expr T -> GenerateIR Var
generateIR_LExpr (EVar _ name) = getVar name
-- generateIR_LExpr (EArrAcc type_ arr expr) = do
--     var <- generateIR_LExpr arr
--     value <- liftM VVar $ generateIR_Expr expr
--     let size = sizeOf type_
--     value <- liftM VVar $ emitIR_ToTemp (\t -> IR_IBinOp IMul t value (VInt size))
--     value <- emitIR_ToTemp (\t -> IR_IBinOp IAdd t (VVar var) value)
--     return value

sizeOf :: Type a -> Integer
sizeOf (Int _) = 4
sizeOf (Bool _) = 1
sizeOf (Void _) = 0
-- sizeOf (Array _ _) = 8
sizeOf (Fun _ _ _) = 8
