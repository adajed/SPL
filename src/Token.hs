module Token (
    CIdent(..),
    VIdent(..)
    ) where

newtype CIdent = CIdent String deriving (Eq, Ord, Read)
instance Show CIdent where
    show (CIdent name) = name

newtype VIdent = VIdent String deriving (Eq, Ord, Read)
instance Show VIdent where
    show (VIdent name) = name
