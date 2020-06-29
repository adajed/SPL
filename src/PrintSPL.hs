{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module PrintSPL where

import AbsSPL
import Data.Char

import Token
import Type


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print CIdent where
  prt _ (CIdent i) = doc (showString ( i))


instance Print VIdent where
  prt _ (VIdent i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print (Program a) where
  prt i e = case e of
    Prog _ topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print (TopDef a) where
  prt i e = case e of
    FnDef _ type_ vident arguments block -> prPrec i 0 (concatD [prt 0 type_,
                                                                 prt 0 vident,
                                                                 doc (showString "("),
                                                                 prt 0 arguments,
                                                                 doc (showString ")"),
                                                                 prt 0 block
                                                                ])
    ClassDef _ cident extends classelems -> prPrec i 0 (concatD [doc (showString "class"),
                                                                 prt 0 cident,
                                                                 doc (showString "{"),
                                                                 prt 0 classelems,
                                                                 doc (showString "}")
                                                                ])
    TypeDef _ cident type_ -> prPrec i 0 (concatD [doc (showString "typedef"),
                                                   prt 0 cident,
                                                   doc (showString "="),
                                                   prt 0 type_,
                                                   doc (showString ";")
                                                  ])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (Argument a) where
  prt i e = case e of
    Arg _ type_ vident -> prPrec i 0 (concatD [prt 0 type_, prt 0 vident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (ClassElem a) where
  prt i e = case e of
    Field _ type_ vidents -> prPrec i 0 (concatD [prt 0 type_, prt 0 vidents, doc (showString ";")])
    Constr _ arguments block -> prPrec i 0 (concatD [doc (showString "constr"),
                                                     doc (showString "("),
                                                     prt 0 arguments,
                                                     doc (showString ")"),
                                                     prt 0 block
                                                    ])
    Method _ type_ vident arguments block -> prPrec i 0 (concatD [prt 0 type_,
                                                                  prt 0 vident,
                                                                  doc (showString "("),
                                                                  prt 0 arguments,
                                                                  doc (showString ")"),
                                                                  prt 0 block])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (Block a) where
  prt i e = case e of
    Bl _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print (Stmt a) where
  prt i e = case e of
    Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    BStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    Ass _ expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "="), prt 0 expr2, doc (showString ";")])
    Incr _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString "++"), doc (showString ";")])
    Decr _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString "--"), doc (showString ";")])
    Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    VRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    Cond _ expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    CondElse _ expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    While _ expr stmt -> prPrec i 0 (concatD [doc (showString "while"),
                                              doc (showString "("),
                                              prt 0 expr,
                                              doc (showString ")"),
                                              prt 0 stmt])
    ForUp _ vident expr1 expr2 expr3 stmt -> prPrec i 0 (concatD [doc (showString "for"),
                                                                  doc (showString "("),
                                                                  prt 0 vident,
                                                                  doc (showString "="),
                                                                  prt 0 expr1,
                                                                  doc (showString "to"),
                                                                  prt 0 expr2,
                                                                  doc (showString "by"),
                                                                  prt 0 expr3,
                                                                  doc (showString ")"),
                                                                  prt 0 stmt])
    ForDown _ vident expr1 expr2 expr3 stmt -> prPrec i 0 (concatD [doc (showString "for"),
                                                                    doc (showString "("),
                                                                    prt 0 vident,
                                                                    doc (showString "="),
                                                                    prt 0 expr1,
                                                                    doc (showString "down"),
                                                                    doc (showString "to"),
                                                                    prt 0 expr2,
                                                                    doc (showString "by"),
                                                                    prt 0 expr3,
                                                                    doc (showString ")"),
                                                                    prt 0 stmt])
    ForEach _ vident expr stmt -> prPrec i 0 (concatD [doc (showString "for"),
                                                            doc (showString "("),
                                                            prt 0 vident,
                                                            doc (showString "in"),
                                                            prt 0 expr,
                                                            doc (showString ")"),
                                                            prt 0 stmt])
    SExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (Item a) where
  prt i e = case e of
    NoInit _ vident -> prPrec i 0 (concatD [prt 0 vident])
    Init _ vident expr -> prPrec i 0 (concatD [prt 0 vident, doc (showString "="), prt 0 expr])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Type a) where
  prt i e = case e of
    Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    Char _ -> prPrec i 0 (concatD [doc (showString "char")])
    Bool _ -> prPrec i 0 (concatD [doc (showString "bool")])
    Void _ -> prPrec i 0 (concatD [doc (showString "void")])
    Class _ cident -> prPrec i 0 (concatD [prt 0 cident])
    Array _ type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]")])
    Fun _ type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])
    Null _ -> prPrec i 0 (concatD [])
    NamedType _ cident -> prPrec i 0 (concatD [prt 0 cident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Expr a) where
  prt i e = case e of
    ETypedExpr _ type_ expr -> prPrec i 6 (concatD [prt 0 type_, prt 6 expr])
    EChar _ char -> prPrec i 6 (concatD [doc (showChar char)])
    EString _ string -> prPrec i 6 (concatD [doc (showString string)])
    ENull _ -> prPrec i 6 (concatD [doc (showString "null")])
    EInt _ n -> prPrec i 6 (concatD [prt 0 n])
    ETrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    EFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    EVar _ vident -> prPrec i 6 (concatD [prt 0 vident])
    EField _ expr vident -> prPrec i 6 (concatD [prt 6 expr, doc (showString "."), prt 0 vident])
    EArrAcc _ expr1 expr2 -> prPrec i 6 (concatD [prt 6 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    EApp _ expr exprs -> prPrec i 6 (concatD [doc (showString "("),
                                              prt 6 expr,
                                              doc (showString ")"),
                                              doc (showString "("),
                                              prt 0 exprs,
                                              doc (showString ")")])
    EUnaryOp _ op expr -> prPrec i 5 (concatD [ doc (showString (show op))
                                              , prt 5 expr])
    EBinOp _ expr1 op expr2 -> prPrec i 4 (concatD [ prt 4 expr1
                                                   , doc (showString (show op))
                                                   , prt 4 expr2])
    EObjNew _ cident exprs -> prPrec i 0 (concatD [doc (showString "new"), prt 0 cident, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EArrNew _ type_ expr -> prPrec i 0 (concatD [doc (showString "new"), prt 0 type_, doc (showString "["), prt 0 expr, doc (showString "]")])
    ELambda _ arguments stmt -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 arguments, doc (showString "->"), prt 0 stmt])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
