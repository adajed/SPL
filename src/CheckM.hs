module CheckM where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import AbsSPL
import LexSPL
import ErrM
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
                                 , level = 0
                                 , currentClass = Nothing
                                 , classGraph = M.empty
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

getArgType :: Argument Pos -> T
getArgType (Arg _ t _) = toUnit t

declareArg :: Argument Pos -> CheckM ()
declareArg (Arg pos t name) = declareVar pos name (toUnit t)

-- building graph of class dependency
buildClassGraph :: [TopDef a] -> CheckM ()
buildClassGraph topdefs = do
    let go m (ClassDef _ cls (Extends _ superCls) _) = M.insert cls superCls m
        go m _ = m
    let g = foldl go M.empty topdefs
    modify (\s -> s { classGraph = g })

declareTopDef :: TopDef Pos -> CheckM ()
declareTopDef (FnDef pos t name args _) =
    declareVar pos name (funT (toUnit t) (map getArgType args))
declareTopDef (ClassDef pos cls (NoExtends _) args) = do
    (fieldEnv, methodEnv, _, constr') <- foldM addClassElem (M.empty, M.empty, S.empty, S.empty) args
    let constr = if S.null constr' then S.singleton [] else constr'
    let info = ClassInfo { fieldTypes = fieldEnv
                         , methodTypes = methodEnv
                         , extends = Nothing
                         , constructors = constr}
    declareClass pos cls info
declareTopDef (ClassDef pos cls (Extends pos' superCls) args) = do
    assertClassNotDeclared pos cls
    superInfo <- getClassInfo pos' superCls
    let f = fieldTypes superInfo
    let m = methodTypes superInfo
    let acc = (f, m, S.empty, S.empty)
    (fieldEnv, methodEnv, _, constr') <- foldM addClassElem acc args
    let constr = if S.null constr' then S.singleton [] else constr'
    let info = ClassInfo { fieldTypes = fieldEnv
                         , methodTypes = methodEnv
                         , extends = Just superCls
                         , constructors = constr}
    declareClass pos cls info

(!!!) :: Show a => Ord a => M.Map a b -> a -> b
m !!! x = case m M.!? x of
            Just y -> y
            Nothing -> error $ "Cannot find " ++ show x



addClassElem :: (TypeEnv, TypeEnv, S.Set VIdent, S.Set [T]) -> ClassElem Pos -> CheckM (TypeEnv, TypeEnv, S.Set VIdent, S.Set [T])
addClassElem (fieldEnv, methodEnv, usedNames, constrs) (Field pos ty names) = do
    let t = toUnit ty
    let m (fEnv, set) name = do
                             let err = errorMsg pos $ show name ++ " already declared"
                             when (S.member name set) err
                             when (M.member name fEnv) err
                             when (M.member name methodEnv) err
                             return (M.insert name t fEnv, S.insert name set)
    (fieldEnv', usedNames') <- foldM m (fieldEnv, usedNames) names
    return (fieldEnv', methodEnv, usedNames', constrs)
addClassElem (fieldEnv, methodEnv, usedNames, constrs) (Method pos ty name args _) = do
    let t = funT (toUnit ty) (map getArgType args)
    let err = errorMsg pos $ show name ++ " already declared"
    when (S.member name usedNames) err
    when (M.member name fieldEnv) err
    when (M.member name methodEnv) (when (t /= methodEnv !!! name) err)
    let methodEnv' = M.insert name t methodEnv
    let usedNames' = S.insert name usedNames
    return (fieldEnv, methodEnv', usedNames', constrs)
addClassElem (fieldEnv, methodEnv, usedNames, constrs) (Constr pos args _) = do
    let t = map getArgType args
    let err = errorMsg pos $ "Redefinition of constructor: " ++ show t
    g <- gets classGraph
    let b = any (\t' -> isSubtypeList g t' t || isSubtypeList g t t') (S.toList constrs)
    when b err
    let constrs' = S.insert t constrs
    return (fieldEnv, methodEnv, usedNames, constrs')


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
