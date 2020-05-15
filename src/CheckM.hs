module CheckM where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import Data.Map as Map

import AbsSPL
import ErrM

type FData = ()

type TType = Type ()

intT, boolT, voidT :: TType
intT  = Basic () (Int ())
boolT = Basic () (Bool ())
voidT = Basic () (Void ())

type VarEnv = Map Ident TType

data SState = RState { rettype :: TType
                     , varenv :: VarEnv }

type CheckM a = StateT SState Err a

initialEnv :: VarEnv
initialEnv = Map.fromList [(Ident "printInt", Fun () voidT [intT])]

runCheckM :: CheckM a -> Err a
runCheckM m = evalStateT m initialSState
    where initialSState = RState { rettype = Basic () (Void ())
                                 , varenv = initialEnv }

getVariableType :: FData -> Ident -> CheckM TType
getVariableType pos name = do
    env <- gets varenv
    case env !? name of
      Just a -> return a
      Nothing -> errorMsg pos ("Variable " ++ show name ++ " does not exists")

getReturnType :: CheckM TType
getReturnType = gets rettype

declareVar :: Ident -> TType -> CheckM ()
declareVar name t =
    modify (\s -> s { varenv = Map.insert name t (varenv s) } )

getArgType :: Argument FData -> TType
getArgType (Arg _ t _) = t

declareArg :: Argument FData -> CheckM ()
declareArg (Arg _ t name) = declareVar name t

declareFunction :: TopDef FData -> CheckM ()
declareFunction (FnDef _ t name args _) =
    declareVar name (Fun () t (Prelude.map getArgType args))

doWithSavedEnv :: CheckM a -> CheckM a
doWithSavedEnv m = do
    env <- gets varenv
    r <- m
    modify (\s -> s { varenv = env })
    return r

errorMsg :: FData -> String -> CheckM a
errorMsg () msg = fail msg
-- errorMsg (Just (x, y)) msg = fail msg'
--     where msg' = (show x) ++ ":" ++ (show y) ++ " " ++ msg
