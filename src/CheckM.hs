module CheckM where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import qualified Data.Map as M

import AbsSPL
import ErrM

type FData = ()
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

assertVariableNotDeclared :: FData -> VIdent -> CheckM ()
assertVariableNotDeclared pos name = do
    info <- liftM (M.!? name) $ gets varEnv
    l <- getCurrentLevel
    case info of
      Just i -> when (l == varLevel i) (errorMsg pos ("Variable " ++ show name ++ " already declared"))
      Nothing -> return ()

assertClassNotDeclared :: FData -> CIdent -> CheckM ()
assertClassNotDeclared pos name = do
    info <- liftM (M.!? name) $ gets classEnv
    case info of
      Just _ -> errorMsg pos ("Class " ++ show name ++ " already declared")
      Nothing -> return ()

assertClassDeclared :: CIdent -> CheckM ()
assertClassDeclared name = do
    info <- liftM (M.!? name) $ gets classEnv
    when (info == Nothing) (errorMsg () ("Class " ++ show name ++ " not declared"))

getVariableInfo :: FData -> VIdent -> CheckM VarInfo
getVariableInfo pos name = do
    info <- liftM (M.!? name) $ gets varEnv
    case info of
      Just i -> return i
      Nothing -> errorMsg pos ("Variable " ++ show name ++ " not declared")

getVariableType :: FData -> VIdent -> CheckM TType
getVariableType pos name = liftM varType $ getVariableInfo pos name

getVariableLevel :: FData -> VIdent -> CheckM Level
getVariableLevel pos name = liftM varLevel $ getVariableInfo pos name

getClassInfo :: CIdent -> CheckM ClassInfo
getClassInfo name = do
    assertClassDeclared name
    liftM (M.! name) $ gets classEnv

getReturnType :: CheckM TType
getReturnType = gets rettype

getCurrentLevel :: CheckM Level
getCurrentLevel = gets level

declareVar :: FData -> VIdent -> TType -> CheckM ()
declareVar pos name t = do
    assertVariableNotDeclared pos name
    l <- getCurrentLevel
    let info = VarInfo { varLevel = l, varType = t }
    modify (\s -> s { varEnv = M.insert name info (varEnv s) })

declareClass :: FData -> CIdent -> ClassInfo -> CheckM ()
declareClass pos name info = do
    assertClassNotDeclared pos name
    modify (\s -> s { classEnv = M.insert name info (classEnv s) })

getArgType :: Argument FData -> TType
getArgType (Arg _ t _) = t

declareArg :: Argument FData -> CheckM ()
declareArg (Arg pos t name) = declareVar pos name t

declareTopDef :: TopDef FData -> CheckM ()
declareTopDef (FnDef pos t name args _) =
    declareVar pos name (funT t (map getArgType args))
declareTopDef (ClDef pos cls args) = do
    let addField t m x = M.insert x t m
    let addArg m (Field _ t xs) = foldl (addField t) m xs
    let fieldMap = foldl addArg M.empty args
    let info = ClassInfo { fieldTypes = fieldMap }
    declareClass pos cls info

doWithSavedEnv :: CheckM a -> CheckM a
doWithSavedEnv m = do
    env <- gets varEnv
    r <- m
    modify (\s -> s { varEnv = env })
    return r

errorMsg :: FData -> String -> CheckM a
errorMsg () msg = fail msg
-- errorMsg (Just (x, y)) msg = fail msg'
--     where msg' = (show x) ++ ":" ++ (show y) ++ " " ++ msg
