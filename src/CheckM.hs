module CheckM where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import Data.Map as Map

import AbsSPL
import ErrM

type FData = ()

type TType = Type ()

intT, boolT, voidT, nullT :: TType
intT = Int ()
boolT = Bool ()
voidT = Void ()
nullT = Null ()

type VarEnv = Map VIdent TType

data SState = SState { rettype :: TType
                     , varenv :: VarEnv
                     , fields :: Map CIdent (Map VIdent TType) }

type CheckM a = StateT SState Err a

initialEnv :: VarEnv
initialEnv = Map.fromList [(VIdent "printInt", Fun () voidT [intT])]

runCheckM :: CheckM a -> Err a
runCheckM m = evalStateT m initialSState
    where initialSState = SState { rettype = voidT
                                 , varenv = initialEnv
                                 , fields = Map.empty }

getVariableType :: FData -> VIdent -> CheckM TType
getVariableType pos name = do
    env <- gets varenv
    case env !? name of
      Just a -> return a
      Nothing -> errorMsg pos ("Variable " ++ show name ++ " does not exists")

getReturnType :: CheckM TType
getReturnType = gets rettype

declareVar :: VIdent -> TType -> CheckM ()
declareVar name t =
    modify (\s -> s { varenv = Map.insert name t (varenv s) } )

getArgType :: Argument FData -> TType
getArgType (Arg _ t _) = t

declareArg :: Argument FData -> CheckM ()
declareArg (Arg _ t name) = declareVar name t

declareTopDef :: TopDef FData -> CheckM ()
declareTopDef (FnDef _ t name args _) =
    declareVar name (Fun () t (Prelude.map getArgType args))
declareTopDef (ClDef _ cls args) = do
    let addField t m x = Map.insert x t m
    let addArg m (Field _ t xs) = Prelude.foldl (addField t) m xs
    let fieldMap = Prelude.foldl addArg Map.empty args
    modify (\s -> s { fields = Map.insert cls fieldMap (fields s) })

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
