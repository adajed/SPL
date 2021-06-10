module CheckM where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import AbsSPL
import LexSPL
import ErrM
import Operator
import Token
import Type

type Level = Int

data VarInfo = VarInfo { varLevel :: Level
                       , varType  :: T
                       }
    deriving (Eq, Ord)

type TypeEnv = M.Map VIdent T

data ClassInfo = ClassInfo { fieldTypes :: TypeEnv
                           , methodTypes :: TypeEnv
                           , extends :: Maybe CIdent
                           , constructors :: S.Set [T]
                           }
    deriving (Eq, Ord)

type VarEnv = M.Map VIdent VarInfo
type ClassEnv = M.Map CIdent ClassInfo

toUnit :: Functor f => f a -> f ()
toUnit = fmap (const ())

toVoid :: Functor f => f a -> f T
toVoid = fmap (const voidT)


data SState = SState { rettype      :: T
                     , varEnv       :: VarEnv
                     , classEnv     :: ClassEnv
                     , level        :: Level
                     , currentClass :: Maybe CIdent
                     , classGraph   :: M.Map CIdent CIdent
                     , typedefs     :: M.Map CIdent T
                     , operatorTypes :: M.Map Operator (T -> Bool)
                     , operatorOutput :: M.Map Operator (T -> T)
                     }

type CheckM a = StateT SState Err a

initialEnv :: VarEnv
initialEnv = M.fromList [ (VIdent "printInt", v 0 (funT voidT [intT]))
                        , (VIdent "print", v 0 (funT voidT [arrayT charT]))
                        ]
    where v n t = VarInfo { varLevel = n, varType = t }

initialOperatorTypes :: M.Map Operator (T -> Bool)
initialOperatorTypes = M.fromList [ (Neg,       (==intT))
                                  , (Not,       (==boolT))
                                  , (BitNot,    (==intT))
                                  , (Or,        (==boolT))
                                  , (And,       (==boolT))
                                  , (Plus,      (==intT))
                                  , (Minus,     (==intT))
                                  , (Times,     (==intT))
                                  , (Div,       (==intT))
                                  , (Mod,       (==intT))
                                  , (LShift,    (==intT))
                                  , (RShift,    (==intT))
                                  , (BitAnd,    (==intT))
                                  , (BitOr,     (==intT))
                                  , (BitXor,    (==intT))
                                  , (Less,      (==intT))
                                  , (LessEq,    (==intT))
                                  , (Greater,   (==intT))
                                  , (GreaterEq, (==intT))
                                  , (Equal,     (const True))
                                  , (NotEqual,  (const True))
                                  ]


initialOperatorOutput :: M.Map Operator (T -> T)
initialOperatorOutput = M.fromList [ (Neg,       id)
                                   , (Not,       id)
                                   , (BitNot,    id)
                                   , (Or,        id)
                                   , (And,       id)
                                   , (Plus,      id)
                                   , (Minus,     id)
                                   , (Times,     id)
                                   , (Div,       id)
                                   , (Mod,       id)
                                   , (LShift,    id)
                                   , (RShift,    id)
                                   , (BitAnd,    id)
                                   , (BitOr,     id)
                                   , (BitXor,    id)
                                   , (Less,      const boolT)
                                   , (LessEq,    const boolT)
                                   , (Greater,   const boolT)
                                   , (GreaterEq, const boolT)
                                   , (Equal,     const boolT)
                                   , (NotEqual,  const boolT)
                                   ]

runCheckM :: CheckM a -> Err a
runCheckM m = evalStateT m initialSState
    where initialSState = SState { rettype = voidT
                                 , varEnv = initialEnv
                                 , classEnv = M.empty
                                 , level = 0
                                 , currentClass = Nothing
                                 , classGraph = M.empty
                                 , typedefs = M.empty
                                 , operatorTypes = initialOperatorTypes
                                 , operatorOutput = initialOperatorOutput
                                 }

assertVariableNotDeclared :: Pos -> VIdent -> CheckM ()
assertVariableNotDeclared pos name = do
    info <- liftM (M.!? name) $ gets varEnv
    l <- getCurrentLevel
    case info of
      Just i -> when (l == varLevel i) (errorMsg pos ("Variable " ++ show name ++ " already declared"))
      Nothing -> return ()

assertClassNotDeclared :: Pos -> CIdent -> CheckM ()
assertClassNotDeclared pos name = do
    info <- liftM (M.!? name) $ gets classEnv
    case info of
      Just _ -> errorMsg pos ("Class " ++ show name ++ " already declared")
      Nothing -> return ()

assertClassDeclared :: Pos -> CIdent -> CheckM ()
assertClassDeclared pos name = do
    info <- liftM (M.!? name) $ gets classEnv
    when (info == Nothing) (errorMsg pos ("Class " ++ show name ++ " not declared"))

getVariableInfo :: Pos -> VIdent -> CheckM VarInfo
getVariableInfo pos name = do
    info <- liftM (M.!? name) $ gets varEnv
    case info of
      Just i -> return i
      Nothing -> errorMsg pos ("Variable " ++ show name ++ " not declared")

getVariableType :: Pos -> VIdent -> CheckM T
getVariableType pos name = liftM varType $ getVariableInfo pos name

getVariableLevel :: Pos -> VIdent -> CheckM Level
getVariableLevel pos name = liftM varLevel $ getVariableInfo pos name

getClassInfo :: Pos -> CIdent -> CheckM ClassInfo
getClassInfo pos name = do
    assertClassDeclared pos name
    liftM (!!! name) $ gets classEnv

getReturnType :: CheckM T
getReturnType = gets rettype

getCurrentLevel :: CheckM Level
getCurrentLevel = gets level

declareVar :: Pos -> VIdent -> T -> CheckM ()
declareVar pos name t = do
    assertVariableNotDeclared pos name
    l <- getCurrentLevel
    let info = VarInfo { varLevel = l, varType = t }
    modify (\s -> s { varEnv = M.insert name info (varEnv s) })

declareClass :: Pos -> CIdent -> ClassInfo -> CheckM ()
declareClass pos name info = do
    assertClassNotDeclared pos name
    modify (\s -> s { classEnv = M.insert name info (classEnv s) })

getArgType :: Argument a -> T
getArgType (Arg _ t _) = toUnit t

declareArg :: Argument Pos -> CheckM ()
declareArg (Arg pos t name) = declareVar pos name (toUnit t)

-- building graph of class dependency
buildClassGraph :: [TopDef a] -> CheckM ()
buildClassGraph topdefs = do
    let go m (ClassDef _ cls (Extends _ superCls) _) = M.insert cls superCls m
        go m _ = m
    let g = foldl go M.empty topdefs
    let go2 m (ClassDef _ cls _ _) = M.insert cls (classT cls) m
        go2 m _ = m
    let defs = foldl go2 M.empty topdefs
    modify (\s -> s { classGraph = g })
    modify (\s -> s { typedefs = defs })

(!!!) :: Show a => Ord a => M.Map a b -> a -> b
m !!! x = case m M.!? x of
            Just y -> y
            Nothing -> error $ "Cannot find " ++ show x



doWithSavedEnv :: CheckM a -> CheckM a
doWithSavedEnv m = do
    env <- gets varEnv
    modify (\s -> s { level = (level s) + 1 })
    r <- m
    modify (\s -> s { varEnv = env })
    modify (\s -> s { level = (level s) - 1 })
    return r

errorMsg :: Pos -> String -> CheckM a
errorMsg Nothing msg = fail msg
errorMsg (Just (x, y)) msg = fail msg'
    where msg' = (show x) ++ ":" ++ (show y) ++ " " ++ msg

assertCanAssign :: Pos -> T -> T -> CheckM ()
assertCanAssign pos t1 t2 = do
    g <- gets classGraph
    let err = errorMsg pos $ show t2 ++ " is not a subtype of " ++ show t1
    unless (isSubtype g t1 t2) err

getCommonType :: Pos -> T -> T -> CheckM T
getCommonType pos t1 t2 = do
    g <- gets classGraph
    case commonSuperType g t1 t2 of
      Nothing -> errorMsg pos $ show t1 ++ " doesn't match " ++ show t2
      Just t -> return t

assertTypeMatchOper :: Pos -> Operator -> T -> CheckM ()
assertTypeMatchOper pos op t = do
    m <- gets operatorTypes
    let err = errorMsg pos $ show t ++ " doesn't match \"" ++ show op ++ "\""
    unless ((m M.! op) t) err

getOutputType :: Operator -> T -> CheckM T
getOutputType op t = do
    m <- gets operatorOutput
    return ((m M.! op) t)
