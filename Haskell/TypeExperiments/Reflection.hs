{-# LANGUAGE GADTs, RankNTypes #-}
module TypeExperiments.Reflection where

import Data.Proxy
import Data.Reflection

data Reified a where
    Reified :: Reifies s a => !(Proxy s) -> Reified a

instance Show a => Show (Reified a) where
    showsPrec p x =
        showParen (p > 10)
            ( showString "banzai "
            . showsPrec 11 (spork x))

banzai :: t -> Reified t
banzai x = reify x Reified

spork :: Reified t -> t
spork (Reified p) = reflect p

shazzam :: Reified a -> (forall s. Reifies s a => Proxy s -> r) -> r
shazzam (Reified p) f = f p

instance Functor Reified where
    fmap f = banzai . f . spork

instance Monad Reified where
    return = banzai
    x >>= f = f (spork x)
