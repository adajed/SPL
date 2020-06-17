module AbsSPL where


newtype CIdent = CIdent String deriving (Eq, Ord, Read)
instance Show CIdent where
    show (CIdent name) = name

newtype VIdent = VIdent String deriving (Eq, Ord, Read)
instance Show VIdent where
    show (VIdent name) = name


-- program
data Program a = Prog a [TopDef a]
  deriving (Eq, Ord, Show, Read)

instance Functor Program where
    fmap f x = case x of
        Prog a topdefs -> Prog (f a) (map (fmap f) topdefs)


-- top level definiton (function, struct, class)
data TopDef a
    = FnDef a (Type a) VIdent [Argument a] (Block a)
    | ClassDef a CIdent (ClassExtends a) [ClassElem a]
  deriving (Eq, Ord, Show, Read)

instance Functor TopDef where
    fmap f x = case x of
        FnDef a type_ vident arguments block -> FnDef (f a) (fmap f type_) vident (map (fmap f) arguments) (fmap f block)
        ClassDef a cident classextends classelems -> ClassDef (f a) cident (fmap f classextends) (map (fmap f) classelems)


-- function argument
data Argument a = Arg a (Type a) VIdent
  deriving (Eq, Ord, Show, Read)

instance Functor Argument where
    fmap f x = case x of
        Arg a type_ vident -> Arg (f a) (fmap f type_) vident


-- class element (field, method)
data ClassElem a
    = Field a (Type a) [VIdent]
    | Method a (Type a) VIdent [Argument a] (Block a)
  deriving (Eq, Ord, Show, Read)

instance Functor ClassElem where
    fmap f x = case x of
        Field a type_ vidents -> Field (f a) (fmap f type_) vidents
        Method a type_ vident arguments block -> Method (f a) (fmap f type_) vident (map (fmap f) arguments) (fmap f block)



-- class extends
data ClassExtends a
    = NoExtends a
    | Extends a CIdent
    deriving (Eq, Ord, Show, Read)

instance Functor ClassExtends where
    fmap f x = case x of
        NoExtends a -> NoExtends (f a)
        Extends a cident -> Extends (f a) cident

-- block of code
data Block a = Bl a [Stmt a]
  deriving (Eq, Ord, Show, Read)

instance Functor Block where
    fmap f x = case x of
        Bl a stmts -> Bl (f a) (map (fmap f) stmts)


-- statement
data Stmt a
    = Empty a
    | BStmt a (Block a)
    | Decl a (Type a) [Item a]
    | Ass a (Expr a) (Expr a)
    | Incr a (Expr a)
    | Decr a (Expr a)
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | ForUp a VIdent (Expr a) (Expr a) (Expr a) (Stmt a)
    | ForDown a VIdent (Expr a) (Expr a) (Expr a) (Stmt a)
    | ForEach a VIdent (Expr a) (Stmt a)
    | SExp a (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Stmt where
    fmap f x = case x of
        Empty a -> Empty (f a)
        BStmt a block -> BStmt (f a) (fmap f block)
        Decl a type_ items -> Decl (f a) (fmap f type_) (map (fmap f) items)
        Ass a expr1 expr2 -> Ass (f a) (fmap f expr1) (fmap f expr2)
        Incr a expr -> Incr (f a) (fmap f expr)
        Decr a expr -> Decr (f a) (fmap f expr)
        Ret a expr -> Ret (f a) (fmap f expr)
        VRet a -> VRet (f a)
        Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
        CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
        While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
        ForUp a vident expr1 expr2 expr3 stmt -> ForUp (f a) vident (fmap f expr1) (fmap f expr2) (fmap f expr3) (fmap f stmt)
        ForDown a vident expr1 expr2 expr3 stmt -> ForDown (f a) vident (fmap f expr1) (fmap f expr2) (fmap f expr3) (fmap f stmt)
        ForEach a vident expr stmt -> ForEach (f a) vident (fmap f expr) (fmap f stmt)
        SExp a expr -> SExp (f a) (fmap f expr)


-- declaration item
data Item a = NoInit a VIdent | Init a VIdent (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Item where
    fmap f x = case x of
        NoInit a vident -> NoInit (f a) vident
        Init a vident expr -> Init (f a) vident (fmap f expr)


-- type
data Type a
    = Int a
    | Bool a
    | Void a
    | Class a CIdent
    | Array a (Type a)
    | Fun a (Type a) [Type a]
    | Null a
  deriving (Eq, Ord, Show, Read)

instance Functor Type where
    fmap f x = case x of
        Int a -> Int (f a)
        Bool a -> Bool (f a)
        Void a -> Void (f a)
        Class a cident -> Class (f a) cident
        Array a type_ -> Array (f a) (fmap f type_)
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)
        Null a -> Null (f a)


-- expression
data Expr a
    = ETypedExpr a (Type a) (Expr a)
    | ENull a
    | EInt a Integer
    | ETrue a
    | EFalse a
    | EVar a VIdent
    | EField a (Expr a) VIdent
    | EArrAcc a (Expr a) (Expr a)
    | EApp a (Expr a) [Expr a]
    | EUnaryOp a (UnaryOp a) (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
    | EObjNew a CIdent
    | EArrNew a (Type a) (Expr a)
    | ELambda a [Argument a] (Stmt a)
  deriving (Eq, Ord, Show, Read)

instance Functor Expr where
    fmap f x = case x of
        ETypedExpr a type_ expr -> ETypedExpr (f a) (fmap f type_) (fmap f expr)
        ENull a -> ENull (f a)
        EInt a integer -> EInt (f a) integer
        ETrue a -> ETrue (f a)
        EFalse a -> EFalse (f a)
        EVar a vident -> EVar (f a) vident
        EField a expr vident -> EField (f a) (fmap f expr) vident
        EArrAcc a expr1 expr2 -> EArrAcc (f a) (fmap f expr1) (fmap f expr2)
        EApp a expr exprs -> EApp (f a) (fmap f expr) (map (fmap f) exprs)
        EUnaryOp a unaryop expr -> EUnaryOp (f a) (fmap f unaryop) (fmap f expr)
        EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
        EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)
        EObjNew a cident -> EObjNew (f a) cident
        EArrNew a type_ expr -> EArrNew (f a) (fmap f type_) (fmap f expr)
        ELambda a arguments stmt -> ELambda (f a) (map (fmap f) arguments) (fmap f stmt)


-- unary op
data UnaryOp a = Neg a | Not a | BitNot a
  deriving (Eq, Ord, Show, Read)


instance Functor UnaryOp where
    fmap f x = case x of
        Neg a -> Neg (f a)
        Not a -> Not (f a)
        BitNot a -> BitNot (f a)


-- add/minus
data AddOp a = Plus a | Minus a
  deriving (Eq, Ord, Show, Read)

instance Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        Minus a -> Minus (f a)


-- multiplication op
data MulOp a
    = Times a
    | Div a
    | Mod a
    | LShift a
    | RShift a
    | BitAnd a
    | BitOr a
    | BitXor a
  deriving (Eq, Ord, Show, Read)

instance Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a -> Div (f a)
        Mod a -> Mod (f a)
        LShift a -> LShift (f a)
        RShift a -> RShift (f a)
        BitAnd a -> BitAnd (f a)
        BitOr a -> BitOr (f a)
        BitXor a -> BitXor (f a)



-- relation op
data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Eq, Ord, Show, Read)

instance Functor RelOp where
    fmap f x = case x of
        LTH a -> LTH (f a)
        LE a -> LE (f a)
        GTH a -> GTH (f a)
        GE a -> GE (f a)
        EQU a -> EQU (f a)
        NE a -> NE (f a)
