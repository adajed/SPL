-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParSPL where
import AbsSPL
import LexSPL
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&' { PT _ (TS _ 4) }
  '&&' { PT _ (TS _ 5) }
  '(' { PT _ (TS _ 6) }
  ')' { PT _ (TS _ 7) }
  '*' { PT _ (TS _ 8) }
  '+' { PT _ (TS _ 9) }
  '++' { PT _ (TS _ 10) }
  ',' { PT _ (TS _ 11) }
  '-' { PT _ (TS _ 12) }
  '--' { PT _ (TS _ 13) }
  '->' { PT _ (TS _ 14) }
  '.' { PT _ (TS _ 15) }
  '/' { PT _ (TS _ 16) }
  ';' { PT _ (TS _ 17) }
  '<' { PT _ (TS _ 18) }
  '<<' { PT _ (TS _ 19) }
  '<=' { PT _ (TS _ 20) }
  '=' { PT _ (TS _ 21) }
  '==' { PT _ (TS _ 22) }
  '>' { PT _ (TS _ 23) }
  '>=' { PT _ (TS _ 24) }
  '>>' { PT _ (TS _ 25) }
  '[' { PT _ (TS _ 26) }
  '[]' { PT _ (TS _ 27) }
  '\\' { PT _ (TS _ 28) }
  ']' { PT _ (TS _ 29) }
  '^' { PT _ (TS _ 30) }
  'bool' { PT _ (TS _ 31) }
  'class' { PT _ (TS _ 32) }
  'else' { PT _ (TS _ 33) }
  'false' { PT _ (TS _ 34) }
  'if' { PT _ (TS _ 35) }
  'int' { PT _ (TS _ 36) }
  'new' { PT _ (TS _ 37) }
  'null' { PT _ (TS _ 38) }
  'return' { PT _ (TS _ 39) }
  'true' { PT _ (TS _ 40) }
  'void' { PT _ (TS _ 41) }
  'while' { PT _ (TS _ 42) }
  '{' { PT _ (TS _ 43) }
  '|' { PT _ (TS _ 44) }
  '||' { PT _ (TS _ 45) }
  '}' { PT _ (TS _ 46) }
  '~' { PT _ (TS _ 47) }

L_integ  { PT _ (TI _) }
L_CIdent { PT _ (T_CIdent _) }
L_VIdent { PT _ (T_VIdent _) }


%%

Integer :: { (Pos, Integer) }
: L_integ {
    (Just (tokenLineCol $1), read (prToken $1))
}

CIdent  :: { (Pos, CIdent) }
: L_CIdent {
    (Just (tokenLineCol $1), CIdent (prToken $1))
}
VIdent  :: { (Pos, VIdent) }
: L_VIdent {
    (Just (tokenLineCol $1), VIdent (prToken $1))
}

Program :: { (Pos, Program Pos) }
Program : ListTopDef { (fst $1, AbsSPL.Prog (fst $1) (snd $1)) }

TopDef :: { (Pos, TopDef Pos) }
TopDef
: Type VIdent '(' ListArgument ')' Block {
    (fst $1, AbsSPL.FnDef (fst $1) (snd $1) (snd $2) (snd $4) (snd $6))
}
| 'class' CIdent '{' ListClassArgument '}' {
    (Just (tokenLineCol $1), AbsSPL.ClDef (Just (tokenLineCol $1)) (snd $2) (reverse (snd $4)))
}

ListTopDef :: { (Pos, [TopDef Pos]) }
ListTopDef
: TopDef {
    (fst $1, (:[]) (snd $1))
}
| TopDef ListTopDef {
    (fst $1, ((:) (snd $1) (snd $2)))
}

Argument :: { (Pos, Argument Pos) }
Argument
: Type VIdent {
    (fst $1, AbsSPL.Arg (fst $1) (snd $1) (snd $2))
}

ListArgument :: { (Pos, [Argument Pos]) }
ListArgument
: {- empty -} {
    (Nothing, [])
}
| Argument {
    (fst $1, [snd $1])
}
| Argument ',' ListArgument {
    (fst $1, (snd $1):(snd $3))
}

ClassArgument :: { (Pos, ClassArgument Pos) }
ClassArgument
: Type ListVIdent ';' {
    (fst $1, AbsSPL.Field (fst $1) (snd $1) (snd $2))
}

ListVIdent :: { (Pos, [VIdent]) }
ListVIdent
: VIdent {
    (fst $1, [snd $1])
}
| VIdent ',' ListVIdent {
    (fst $1, (snd $1):(snd $3))
}

ListClassArgument :: { (Pos, [ClassArgument Pos]) }
ListClassArgument
: {- empty -} {
    (Nothing, [])
}
| ListClassArgument ClassArgument {
    (fst $1, (snd $2):(snd $1))
}

Block :: { (Pos, Block Pos) }
Block
: '{' ListStmt '}' {
    (Just (tokenLineCol $1), AbsSPL.Bl (Just (tokenLineCol $1)) (reverse (snd $2)))
}

ListStmt :: { (Pos, [Stmt Pos]) }
ListStmt
: {- empty -} {
    (Nothing, [])
}
| ListStmt Stmt {
    (fst $1, (snd $2):(snd $1))
}

Stmt :: { (Pos, Stmt Pos) }
Stmt
: ';' {
    (Just (tokenLineCol $1), AbsSPL.Empty (Just (tokenLineCol $1)))
}
| Block {
    (fst $1, AbsSPL.BStmt (fst $1) (snd $1))
}
| Type ListItem ';' {
    (fst $1, AbsSPL.Decl (fst $1) (snd $1) (snd $2))
}
| Expr '=' Expr ';' {
    (fst $1, AbsSPL.Ass (fst $1) (snd $1) (snd $3))
}
| Expr '++' ';' {
    (fst $1, AbsSPL.Incr (fst $1) (snd $1))
}
| Expr '--' ';' {
    (fst $1, AbsSPL.Decr (fst $1) (snd $1))
}
| 'return' Expr ';' {
    (Just (tokenLineCol $1), AbsSPL.Ret (Just (tokenLineCol $1)) (snd $2))
}
| 'return' ';' {
    (Just (tokenLineCol $1), AbsSPL.VRet (Just (tokenLineCol $1)))
}
| 'if' '(' Expr ')' Stmt {
    (Just (tokenLineCol $1), AbsSPL.Cond (Just (tokenLineCol $1)) (snd $3) (snd $5))
}
| 'if' '(' Expr ')' Stmt 'else' Stmt {
    (Just (tokenLineCol $1), AbsSPL.CondElse (Just (tokenLineCol $1)) (snd $3) (snd $5) (snd $7))
}
| 'while' '(' Expr ')' Stmt {
    (Just (tokenLineCol $1), AbsSPL.While (Just (tokenLineCol $1)) (snd $3) (snd $5))
}
| Expr ';' {
    (fst $1, AbsSPL.SExp (fst $1) (snd $1))
}

Item :: { (Pos, Item Pos) }
Item
: VIdent {
    (fst $1, AbsSPL.NoInit (fst $1) (snd $1))
}
| VIdent '=' Expr {
    (fst $1, AbsSPL.Init (fst $1) (snd $1) (snd $3))
}

ListItem :: { (Pos, [Item Pos]) }
ListItem
: Item {
    (fst $1, [snd $1])
}
| Item ',' ListItem {
    (fst $1, (snd $1):(snd $3))
}

Type :: { (Pos, Type Pos) }
Type
: 'int' {
    (Just (tokenLineCol $1), AbsSPL.Int (Just (tokenLineCol $1)))
}
| 'bool' {
    (Just (tokenLineCol $1), AbsSPL.Bool (Just (tokenLineCol $1)))
}
| 'void' {
    (Just (tokenLineCol $1), AbsSPL.Void (Just (tokenLineCol $1)))
}
| CIdent {
    (fst $1, AbsSPL.Class (fst $1) (snd $1))
}
| Type '[]' {
    (fst $1, AbsSPL.Array (fst $1) (snd $1))
}
| Type '(' ListType ')' {
    (fst $1, AbsSPL.Fun (fst $1) (snd $1) (snd $3))
}

ListType :: { (Pos, [Type Pos]) }
ListType
: {- empty -} {
    (Nothing, [])
}
| Type {
    (fst $1, [snd $1])
}
| Type ',' ListType {
    (fst $1, (snd $1):(snd $3))
}

Expr6 :: { (Pos, Expr Pos) }
Expr6
: 'null' {
    (Just (tokenLineCol $1), AbsSPL.ENull (Just (tokenLineCol $1)))
}
| Integer {
    (fst $1, AbsSPL.EInt (fst $1) (snd $1))
}
| 'true' {
    (Just (tokenLineCol $1), AbsSPL.ETrue (Just (tokenLineCol $1)))
}
| 'false' {
    (Just (tokenLineCol $1), AbsSPL.EFalse (Just (tokenLineCol $1)))
}
| VIdent {
    (fst $1, AbsSPL.EVar (fst $1) (snd $1))
}
| Expr6 '.' VIdent {
    (fst $1, AbsSPL.EField (fst $1) (snd $1) (snd $3))
}
| Expr6 '[' Expr ']' {
    (fst $1, AbsSPL.EArrAcc (fst $1) (snd $1) (snd $3))
}
| Expr6 '(' ListExpr ')' {
    (fst $1, AbsSPL.EApp (fst $1) (snd $1) (snd $3))
}
| '(' Expr ')' {
    (Just (tokenLineCol $1), (snd $2))
}

Expr5 :: { (Pos, Expr Pos) }
Expr5
: UnaryOp Expr5 {
    (fst $1, AbsSPL.EUnaryOp (fst $1) (snd $1) (snd $2))
}
| Expr6 {
    $1
}

Expr4 :: { (Pos, Expr Pos) }
Expr4
: Expr4 MulOp Expr5 {
    (fst $1, AbsSPL.EMul (fst $1) (snd $1) (snd $2) (snd $3))
}
| Expr5 {
    $1
}

Expr3 :: { (Pos, Expr Pos) }
Expr3
: Expr3 AddOp Expr4 {
    (fst $1, AbsSPL.EAdd (fst $1) (snd $1) (snd $2) (snd $3))
}
| Expr4 {
    $1
}

Expr2 :: { (Pos, Expr Pos) }
Expr2
: Expr2 RelOp Expr3 {
    (fst $1, AbsSPL.ERel (fst $1) (snd $1) (snd $2) (snd $3))
}
| Expr3 {
    $1
}

Expr1 :: { (Pos, Expr Pos) }
Expr1
: Expr2 '&&' Expr1 {
    (fst $1, AbsSPL.EAnd (fst $1) (snd $1) (snd $3))
}
| Expr2 {
    $1
}

Expr :: { (Pos, Expr Pos) }
Expr
: Expr1 '||' Expr {
    (fst $1, AbsSPL.EOr (fst $1) (snd $1) (snd $3))
}
| 'new' CIdent {
    (Just (tokenLineCol $1), AbsSPL.EObjNew (Just (tokenLineCol $1)) (snd $2))
}
| 'new' Type '[' Expr ']' {
    (Just (tokenLineCol $1), AbsSPL.EArrNew (Just (tokenLineCol $1)) (snd $2) (snd $4))
}
| '\\' ListArgument '->' Stmt {
    (Just (tokenLineCol $1), AbsSPL.ELambda (Just (tokenLineCol $1)) (snd $2) (snd $4))
}
| Expr1 {
    $1
}

ListExpr :: { (Pos, [Expr Pos]) }
ListExpr
: {- empty -} {
    (Nothing, [])
}
| Expr {
    (fst $1, [snd $1])
}
| Expr ',' ListExpr {
    (fst $1, (snd $1):(snd $3))
}

UnaryOp :: { (Pos, UnaryOp Pos) }
UnaryOp
: '-' {
    (Just (tokenLineCol $1), AbsSPL.Neg (Just (tokenLineCol $1)))
}
| '!' {
    (Just (tokenLineCol $1), AbsSPL.Not (Just (tokenLineCol $1)))
}
| '~' {
    (Just (tokenLineCol $1), AbsSPL.BitNot (Just (tokenLineCol $1)))
}

AddOp :: { (Pos, AddOp Pos) }
AddOp
: '+' {
    (Just (tokenLineCol $1), AbsSPL.Plus (Just (tokenLineCol $1)))
}
| '-' {
    (Just (tokenLineCol $1), AbsSPL.Minus (Just (tokenLineCol $1)))
}

MulOp :: { (Pos, MulOp Pos) }
MulOp
: '*' {
    (Just (tokenLineCol $1), AbsSPL.Times (Just (tokenLineCol $1)))
}
| '/' {
    (Just (tokenLineCol $1), AbsSPL.Div (Just (tokenLineCol $1)))
}
| '%' {
    (Just (tokenLineCol $1), AbsSPL.Mod (Just (tokenLineCol $1)))
}
| '<<' {
    (Just (tokenLineCol $1), AbsSPL.LShift (Just (tokenLineCol $1)))
}
| '>>' {
    (Just (tokenLineCol $1), AbsSPL.RShift (Just (tokenLineCol $1)))
}
| '&' {
    (Just (tokenLineCol $1), AbsSPL.BitAnd (Just (tokenLineCol $1)))
}
| '|' {
    (Just (tokenLineCol $1), AbsSPL.BitOr (Just (tokenLineCol $1)))
}
| '^' {
    (Just (tokenLineCol $1), AbsSPL.BitXor (Just (tokenLineCol $1)))
}

RelOp :: { (Pos, RelOp Pos) }
RelOp
: '<' {
    (Just (tokenLineCol $1), AbsSPL.LTH (Just (tokenLineCol $1)))
}
| '<=' {
    (Just (tokenLineCol $1), AbsSPL.LE (Just (tokenLineCol $1)))
}
| '>' {
    (Just (tokenLineCol $1), AbsSPL.GTH (Just (tokenLineCol $1)))
}
| '>=' {
    (Just (tokenLineCol $1), AbsSPL.GE (Just (tokenLineCol $1)))
}
| '==' {
    (Just (tokenLineCol $1), AbsSPL.EQU (Just (tokenLineCol $1)))
}
| '!=' {
    (Just (tokenLineCol $1), AbsSPL.NE (Just (tokenLineCol $1)))
}

{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before '" ++ id(prToken t) ++ "'"

myLexer = tokens

parseProgram :: [Token] -> Err (Program Pos)
parseProgram = (>>= return . snd) . pProgram

}

