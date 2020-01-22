module CheckM where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import Data.Map as Map

import AbsSPL
import ErrM

type FData = ()

type VarEnv = Map Ident (Type ())

data SState = RState { rettype :: Type ()
                     , varenv :: VarEnv }

type CheckM a = StateT SState Err a

runCheckM :: CheckM a -> Err a
runCheckM m = evalStateT m initialSState
    where initialSState = RState { rettype = Basic () (Void ()), varenv = Map.empty }

getVariableType :: FData -> Ident -> CheckM (Type ())
getVariableType pos name = do
    env <- gets varenv
    case env !? name of
      Just a -> return a
      Nothing -> errorMsg pos ("Variable " ++ show name ++ " does not exists")

getReturnType :: CheckM (Type ())
getReturnType = gets rettype

declareVar :: Ident -> Type () -> CheckM ()
declareVar name t = do
    env <- gets varenv
    modify (\s -> s { varenv = Map.insert name t env } )

errorMsg :: FData -> String -> CheckM a
errorMsg () msg = fail msg
-- errorMsg (Just (x, y)) msg = fail msg'
--     where msg' = (show x) ++ ":" ++ (show y) ++ " " ++ msg
