module SkelSPL where

-- Haskell module generated by the BNF converter

import AbsSPL
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Show a => Program a -> Result
transProgram x = case x of
  Prog _ topdefs -> failure x
transTopDef :: Show a => TopDef a -> Result
transTopDef x = case x of
  FnDef _ type_ ident arguments block -> failure x
transArgument :: Show a => Argument a -> Result
transArgument x = case x of
  Arg _ type_ ident -> failure x
transBlock :: Show a => Block a -> Result
transBlock x = case x of
  Bl _ stmts -> failure x
transStmt :: Show a => Stmt a -> Result
transStmt x = case x of
  Empty _ -> failure x
  BStmt _ block -> failure x
  Decl _ type_ items -> failure x
  Ass _ expr1 expr2 -> failure x
  Incr _ expr -> failure x
  Decr _ expr -> failure x
  Ret _ expr -> failure x
  VRet _ -> failure x
  Cond _ expr stmt -> failure x
  CondElse _ expr stmt1 stmt2 -> failure x
  While _ expr stmt -> failure x
  SExp _ expr -> failure x
transItem :: Show a => Item a -> Result
transItem x = case x of
  NoInit _ ident -> failure x
  Init _ ident expr -> failure x
transType :: Show a => Type a -> Result
transType x = case x of
  Int _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Fun _ type_ types -> failure x
transExpr :: Show a => Expr a -> Result
transExpr x = case x of
  ETypedExpr _ type_ expr -> failure x
  EInt _ integer -> failure x
  ETrue _ -> failure x
  EFalse _ -> failure x
  EVar _ ident -> failure x
  EApp _ expr exprs -> failure x
  EUnaryOp _ unaryop expr -> failure x
  EMul _ expr1 mulop expr2 -> failure x
  EAdd _ expr1 addop expr2 -> failure x
  ERel _ expr1 relop expr2 -> failure x
  EAnd _ expr1 expr2 -> failure x
  EOr _ expr1 expr2 -> failure x
transUnaryOp :: Show a => UnaryOp a -> Result
transUnaryOp x = case x of
  Neg _ -> failure x
  Not _ -> failure x
  BitNot _ -> failure x
transAddOp :: Show a => AddOp a -> Result
transAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x
transMulOp :: Show a => MulOp a -> Result
transMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x
  LShift _ -> failure x
  RShift _ -> failure x
  BitAnd _ -> failure x
  BitOr _ -> failure x
  BitXor _ -> failure x
transRelOp :: Show a => RelOp a -> Result
transRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x

