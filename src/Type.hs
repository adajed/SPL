module Type (
    Type(..),
    T,
    intT, boolT, voidT, nullT,
    arrayT, funT, classT,
    isSubtype,
    isSubtypeList
    ) where

import Token

import qualified Data.Map as M

import Data.List ( intercalate )

-- type
data Type a
    = Int a
    | Bool a
    | Void a
    | Class a CIdent
    | Array a (Type a)
    | Fun a (Type a) [Type a]
    | Null a
  deriving (Eq, Ord, Read)

instance Show (Type a) where
    show t = case t of
               Int _  -> "int"
               Bool _ -> "bool"
               Void _ -> "void"
               Class _ cls -> show cls
               Array _ t -> show t ++ "[]"
               Fun _ tOut tArgs -> show tOut ++ "(" ++ intercalate ", " (map show tArgs) ++ ")"
               Null _ -> "null"

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
