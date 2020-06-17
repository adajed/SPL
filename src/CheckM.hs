module CheckM where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import qualified Data.Map as M

import AbsSPL
import LexSPL
import ErrM

type TType = Type ()
type Level = Int

data VarInfo = VarInfo { varLevel :: Level
                       , varType  :: TType
                       }
    deriving (Eq, Ord)

data ClassInfo = ClassInfo { fieldTypes :: M.Map VIdent TType }
    deriving (Eq, Ord)

type VarEnv = M.Map VIdent VarInfo
type ClassEnv = M.Map CIdent ClassInfo

intT, boolT, voidT, nullT :: TType
intT = Int ()
boolT = Bool ()
voidT = Void ()
nullT = Null ()

arrayT :: TType -> TType
arrayT = Array ()

funT :: TType -> [TType] -> TType
funT = Fun ()

toUnit :: Functor f => f a -> f ()
toUnit = fmap (const ())

toVoid :: Functor f => f a -> f TType
toVoid = fmap (const voidT)


data SState = SState { rettype :: TType
                     , varEnv  :: VarEnv
                     , classEnv  :: ClassEnv
                     , level   :: Level
                     }

type CheckM a = StateT SState Err a

initialEnv :: VarEnv
initialEnv = M.fromList [(VIdent "printInt", VarInfo { varLevel = 0
                                                     , varType  = funT voidT [intT] })
                        ]

runCheckM :: CheckM a -> Err a
runCheckM m = evalStateT m initialSState
    where initialSState = SState { rettype = voidT
                                 , varEnv = initialEnv
                                 , classEnv = M.empty
                                 , level = 0}

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

getVariableType :: Pos -> VIdent -> CheckM TType
getVariableType pos name = liftM varType $ getVariableInfo pos name

getVariableLevel :: Pos -> VIdent -> CheckM Level
getVariableLevel pos name = liftM varLevel $ getVariableInfo pos name

getClassInfo :: Pos -> CIdent -> CheckM ClassInfo
getClassInfo pos name = do
    assertClassDeclared pos name
    liftM (M.! name) $ gets classEnv

getReturnType :: CheckM TType
getReturnType = gets rettype

getCurrentLevel :: CheckM Level
getCurrentLevel = gets level

declareVar :: Pos -> VIdent -> TType -> CheckM ()
declareVar pos name t = do
    assertVariableNotDeclared pos name
    l <- getCurrentLevel
    let info = VarInfo { varLevel = l, varType = t }
    modify (\s -> s { varEnv = M.insert name info (varEnv s) })

declareClass :: Pos -> CIdent -> ClassInfo -> CheckM ()
declareClass pos name info = do
    assertClassNotDeclared pos name
    modify (\s -> s { classEnv = M.insert name info (classEnv s) })

getArgType :: Argument Pos -> TType
getArgType (Arg _ t _) = toUnit t

declareArg :: Argument Pos -> CheckM ()
declareArg (Arg pos t name) = declareVar pos name (toUnit t)

declareTopDef :: TopDef Pos -> CheckM ()
declareTopDef (FnDef pos t name args _) =
    declareVar pos name (funT (toUnit t) (map getArgType args))
declareTopDef (ClassDef pos cls extends args) = do
    let addField t m x = M.insert x t m
    let addArg m (Field _ t xs) = foldl (addField (toUnit t)) m xs
    let fieldMap = foldl addArg M.empty args
    let info = ClassInfo { fieldTypes = fieldMap }
    declareClass pos cls info

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
