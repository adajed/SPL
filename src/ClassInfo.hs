module ClassInfo (
    ClassAttr(..),
    ClassInfo(..),
    self,
    getAllFields,
    getAllMethods,
    methodName,
    vtableName,
    allocatorName,
    constructorName,
    destructorName,
    lambdaEnvName,
    isLambdaEnv
    ) where

import Token
import Type

import qualified Data.Map as M
import qualified Data.List as L


type Offset = Int

data ClassAttr = CField Offset Int | CMethod Offset VIdent
    deriving (Eq, Show)

data ClassInfo = ClassInfo { attrs       :: M.Map VIdent ClassAttr
                           , fieldType   :: M.Map VIdent T
                           , classSize   :: Int
                           , vtableSize  :: Int
                           , constructors :: M.Map [T] VIdent
                           }
    deriving (Eq, Show)

isField :: ClassAttr -> Bool
isField (CField _ _) = True
isField _ = False

isMethod :: ClassAttr -> Bool
isMethod (CMethod _ _) = True
isMethod _ = False

self :: VIdent
self = VIdent "self"

getAllFields :: ClassInfo -> [VIdent]
getAllFields info = M.keys (M.filter isField (attrs info))

getAllMethods :: ClassInfo -> [VIdent]
getAllMethods info = M.keys (M.filter isMethod (attrs info))

methodName :: CIdent -> VIdent -> VIdent
methodName clsName methodName = VIdent $ "__" ++ show methodName ++ "_" ++ show clsName

vtableName :: CIdent -> VIdent
vtableName clsName = VIdent $ "__vtable_" ++ show clsName

allocatorName :: CIdent -> VIdent
allocatorName cls = VIdent $ "__allocate_" ++ show cls

constructorName :: CIdent -> Int -> VIdent
constructorName cls n = VIdent $ "__constructor_" ++ show cls ++ "_" ++ show n

destructorName :: CIdent -> VIdent
destructorName cls = VIdent $ "__destructor_" ++ show cls

lambdaEnvName :: Int -> CIdent
lambdaEnvName n = CIdent $ "env__class__" ++ show n

isLambdaEnv :: CIdent -> Bool
isLambdaEnv cls = L.isPrefixOf "env__class__" (show cls)
