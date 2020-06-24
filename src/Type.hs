module Type (
    Type(..),
    T,
    intT, boolT, voidT, nullT,
    arrayT, funT, classT,
    isSubtype,
    isSubtypeList,
    commonSuperType
    ) where

import Token

import qualified Data.Map as M

import Data.List ( intercalate )

-- type
data Type a
    = Int a
    | Bool a
    | Char a
    | Void a
    | Class a CIdent
    | Array a (Type a)
    | Fun a (Type a) [Type a]
    | Null a
    | NamedType a CIdent
  deriving (Eq, Ord, Read)

instance Show (Type a) where
    show t = case t of
               Int _  -> "int"
               Char _ -> "char"
               Bool _ -> "bool"
               Void _ -> "void"
               Class _ cls -> "(class)" ++ show cls
               Array _ t -> show t ++ "[]"
               Fun _ tOut tArgs -> show tOut ++ "(" ++ intercalate ", " (map show tArgs) ++ ")"
               Null _ -> "null"
               NamedType _ name -> "(typedef)" ++ show name

type T = Type ()

instance Functor Type where
    fmap f x = case x of
        Int a -> Int (f a)
        Bool a -> Bool (f a)
        Void a -> Void (f a)
        Class a cident -> Class (f a) cident
        Array a type_ -> Array (f a) (fmap f type_)
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)
        Null a -> Null (f a)
        NamedType a cident -> NamedType (f a) cident


intT, boolT, voidT, nullT :: T
intT = Int ()
boolT = Bool ()
voidT = Void ()
nullT = Null ()

arrayT :: T -> T
arrayT = Array ()

funT :: T -> [T] -> T
funT = Fun ()

classT :: CIdent -> T
classT = Class ()

isSubtype :: M.Map CIdent CIdent -> Type a -> Type a -> Bool
isSubtype _ (Class _ _) (Null _) = True
isSubtype _ (Fun _ _ _) (Null _) = True
isSubtype _ (Array _ _) (Null _) = True
isSubtype g (Class _ superCls) (Class _ cls) =
    if superCls == cls
       then True
       else case g M.!? cls of
              Just nextCls -> isSubtype g (Class () superCls) (Class () nextCls)
              Nothing -> False
isSubtype g (Array _ t1) (Array _ t2) = isSubtype g t1 t2
isSubtype g (Fun _ tOut1 tArgs1) (Fun _ tOut2 tArgs2) =
        isSubtype g tOut1 tOut2 && isSubtypeList g tArgs2 tArgs1
isSubtype _ (Int _) (Int _) = True
isSubtype _ (Bool _) (Bool _) = True
isSubtype _ (Void _) (Void _) = True
isSubtype _ _ _ = False

isSubtypeList :: M.Map CIdent CIdent -> [Type a] -> [Type a] -> Bool
isSubtypeList g l1 l2 = length l1 == length l2 && and (zipWith (isSubtype g) l1 l2)

commonSuperType :: M.Map CIdent CIdent -> T -> T -> Maybe T
commonSuperType g t@(Class _ _) (Null _) = return t
commonSuperType g t@(Array _ _) (Null _) = return t
commonSuperType g t@(Fun _ _ _) (Null _) = return t
commonSuperType g (Null _) t@(Class _ _) = return t
commonSuperType g (Null _) t@(Array _ _) = return t
commonSuperType g (Null _) t@(Fun _ _ _) = return t
commonSuperType g (Class _ cls1) (Class _ cls2) = do
    superCls <- searchCommonAncestor g cls1 cls2
    return (classT superCls)
commonSuperType g (Array _ t1) (Array _ t2) = do
    superType <- commonSuperType g t1 t2
    return (arrayT superType)
commonSuperType g (Fun _ t1 ts1) (Fun _ t2 ts2) = do
    if length ts1 == length ts2
       then do
           t <- commonSuperType g t1 t2
           ts <- mapM (uncurry (commonSuperType g)) (zip ts1 ts2)
           return (funT t ts)
       else Nothing
commonSuperType _ (Int ()) (Int ()) = return (Int ())
commonSuperType _ (Bool ()) (Bool ()) = return (Bool ())
commonSuperType _ (Void ()) (Void ()) = return (Void ())
commonSuperType _ (Null ()) (Null ()) = return (Null ())
commonSuperType _ _ _ = Nothing

searchCommonAncestor :: M.Map CIdent CIdent -> CIdent -> CIdent -> Maybe CIdent
searchCommonAncestor g cls1 cls2 =
    let l1 = getAncestorList g cls1
        l2 = getAncestorList g cls2
     in getFurthestCommon l1 l2

getAncestorList :: M.Map CIdent CIdent -> CIdent -> [CIdent]
getAncestorList g cls = go cls [cls]
    where go name acc = case g M.!? name of
                          Nothing -> acc
                          Just superCls -> go superCls (superCls:acc)


getFurthestCommon :: [CIdent] -> [CIdent] -> Maybe CIdent
getFurthestCommon [] _ = Nothing
getFurthestCommon _ [] = Nothing
getFurthestCommon (c1:cs1) (c2:cs2) =
    if c1 == c2
       then case getFurthestCommon cs1 cs2 of
              Nothing -> Just c1
              Just c -> Just c
       else Nothing
