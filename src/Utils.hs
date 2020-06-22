module Utils (
    voidT,
    toUnit,
    toVoid
    ) where

import Type

toUnit :: Functor f => f a -> f ()
toUnit = fmap (const ())

toVoid :: Functor f => f a -> f (Type ())
toVoid = fmap (const voidT)
