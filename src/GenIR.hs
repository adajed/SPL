module GenIR where

import AbsSPL
import ClassInfo
import ErrM
import IR
import Token
import Type
import Utils

import Control.Monad.Trans.State
import Control.Monad ( liftM, foldM, when, unless, void )
import Control.Monad.Trans ( lift )

import qualified Data.Map as M

(!!!) :: Show a => Ord a => Show b => M.Map a b -> a -> b
m !!! x = case m M.!? x of
            Just y -> y
            Nothing -> error $ "Cannot find " ++ show x ++ " in " ++ show m

type Offset = Int

type VarEnv = M.Map VIdent (SVar, T)

data SState = SState { tempCounter :: Int
                     , levelCounter :: Int
                     , labelCounter :: Int
                     , classInfo :: M.Map CIdent ClassInfo
                     , varenv :: [VarEnv]
                     , currentOutput :: [IR]
                     , lambdas :: [(Type T, VIdent, [Argument T], Stmt T)]
                     , lambdaCounter :: Int
                     , classGraph :: M.Map CIdent CIdent
                     , output :: ProgramIR
                     }

type GenIR a = StateT SState Err a

initialSState :: SState
initialSState = SState { tempCounter  = 0
                       , levelCounter = 0
                       , labelCounter = 0
                       , classInfo = M.empty
                       , varenv = []
                       , output = ProgramIR { textSection = M.empty, dataSection = M.empty }
                       , currentOutput = []
                       , lambdas = []
                       , lambdaCounter = 0
                       , classGraph = M.empty
                       }

data ProgramIR = ProgramIR { textSection :: M.Map VIdent [IR]
                           , dataSection :: M.Map VIdent [DataIR]
                           }

runGenIR :: GenIR a -> Err ProgramIR
runGenIR m = liftM output $ execStateT m initialSState


---------------------------------------------------------------------------
-- utility functions

-- return size of type represenation (in bytes)
sizeOf :: Type a -> Int
sizeOf (Int _) = 4
sizeOf (Bool _) = 1
sizeOf (Void _) = 0
sizeOf (Array _ _) = 8
sizeOf (Fun _ _ _) = 8
sizeOf (Class _ _) = 8
sizeOf (Null _) = 8

--
getFreshTemp :: GenIR Var
getFreshTemp = do
    i <- gets tempCounter
    modify (\s -> s { tempCounter = i + 1 })
    return (VarT i)

--
getFreshLabel :: GenIR VIdent
getFreshLabel = do
    i <- gets labelCounter
    modify (\s -> s { labelCounter = i + 1 } )
    return $ VIdent $ ".L" ++ show i

-- push new variable env on the stack
pushEmptyEnv :: GenIR ()
pushEmptyEnv = modify (\s -> s { levelCounter = (levelCounter s) + 1
                               , varenv = M.empty:(varenv s)} )

-- pop variable env
popEnv :: GenIR (M.Map VIdent (SVar, T))
popEnv = do
    env <- liftM head $ gets varenv
    modify (\s -> s { levelCounter = (levelCounter s) - 1
                    , varenv = tail (varenv s) })
    return env

-- modify current variable env
modifyTopEnv :: (VarEnv -> VarEnv) -> GenIR ()
modifyTopEnv f = do
    env <- liftM head $ gets varenv
    modify (\s -> s { varenv = (f env):(tail (varenv s)) })

--
emitIR :: IR -> GenIR ()
emitIR ir = modify (\s -> s { currentOutput = ir:(currentOutput s) } )

--
emitIR_ToTemp :: Int -> (SVar -> IR) -> GenIR SVar
emitIR_ToTemp size f = do
    t <- getFreshTemp
    let v = SVar t size
    emitIR (f v)
    return v

--
declareVar :: T -> VIdent -> GenIR SVar
declareVar t name = do
    l <- gets levelCounter
    let name' = VIdent (show name ++ "_" ++ show l)
    let v = SVar (VarN name') (sizeOf t)
    modifyTopEnv (M.insert name (v, t))
    return v

--
declareFunction :: VIdent -> T -> [Argument T] -> GenIR ()
declareFunction name t args = return ()

--
declareClass :: CIdent -> ClassExtends T -> [ClassElem T] -> GenIR ()
declareClass cls (NoExtends _) elems =
    let initInfo = ClassInfo { attrs = M.empty
                             , fieldType = M.empty
                             , classSize = 20
                             , vtableSize = 0
                             , constructors = M.empty}
        info' = foldl (generateClassInfo cls) initInfo elems
        newinfo = insertDefaultConstructor cls info'
     in do
         modify (\s -> s { classInfo = M.insert cls newinfo (classInfo s) })
declareClass cls (Extends _ superCls) elems = do
    info <- liftM (M.! superCls) $ gets classInfo
    let info' = info { constructors = M.empty }
    let info'' = foldl (generateClassInfo cls) info' elems
    let newinfo = insertDefaultConstructor cls info''
    modify (\s -> s { classInfo = M.insert cls newinfo (classInfo s) })

generateClassInfo :: CIdent -> ClassInfo -> ClassElem T -> ClassInfo
generateClassInfo _ info (Field _ t names) = foldl go info names
    where size = sizeOf t
          go :: ClassInfo -> VIdent -> ClassInfo
          go acc name = acc { attrs = M.insert name (CField (classSize acc) size) (attrs acc)
                            , fieldType = M.insert name (fmap (const ()) t) (fieldType acc)
                            , classSize = (classSize acc) + size
                            }
generateClassInfo cls acc (Method _ t name args _) =
    let f = Fun () (fmap (const ()) t) (map (\(Arg _ t' _) -> toUnit t') args)
        mName = methodName cls name
     in if M.member name (attrs acc)
           then let (CMethod offset _) = (attrs acc) M.! name
                  in acc { attrs = M.insert name (CMethod offset mName) (attrs acc) }
           else let offset = vtableSize acc
                 in acc { attrs = M.insert name (CMethod offset mName) (attrs acc)
                        , fieldType = M.insert name f (fieldType acc)
                        , vtableSize = (vtableSize acc) + 8
                        }
generateClassInfo cls acc (Constr _ args _) =
    let ts = map (\(Arg _ t' _) -> toUnit t') args
        name = constructorName cls (M.size (constructors acc))
     in acc { constructors = M.insert ts name (constructors acc) }

insertDefaultConstructor :: CIdent -> ClassInfo -> ClassInfo
insertDefaultConstructor cls info = info { constructors = c' }
    where name = allocatorName cls
          c = constructors info
          c' = if M.null c then M.singleton [] name else c


-- building graph of class dependency
buildClassGraph :: [TopDef a] -> GenIR ()
buildClassGraph topdefs = do
    let go m (ClassDef _ cls (Extends _ superCls) _) = M.insert cls superCls m
        go m _ = m
    let g = foldl go M.empty topdefs
    modify (\s -> s { classGraph = g })


-- search stack of envs for variable
getMaybeVar :: VIdent -> GenIR (Maybe SVar)
getMaybeVar name = do
    let f [] = return Nothing
        f (e:es) = if M.member name e
                      then return (Just (fst (e M.! name)))
                      else f es
    f =<< gets varenv

--
getVar :: VIdent -> GenIR SVar
getVar name = do
    x <- getMaybeVar name
    case x of
      Just var -> return var
      Nothing -> errorVariableNotDeclared name

--
getVarType :: VIdent -> GenIR T
getVarType name = do
    let f [] = errorVariableNotDeclared name
        f (e:es) = if M.member name e
                      then return (snd (e M.! name))
                      else f es
    f =<< gets varenv


-- save current output
saveCurrentOutput :: VIdent -> GenIR ()
saveCurrentOutput name = do
    ir <- liftM reverse $ gets currentOutput
    modify (\s -> s { output = (output s) { textSection = M.insert name ir (textSection (output s)) } })

------------------------------------------------------------------------
-- errors

errorVariableNotDeclared :: VIdent -> GenIR a
errorVariableNotDeclared name =
    fail ("Variable " ++ show name ++ " not declared")

