{-# LANGUAGE 
        FlexibleInstances,
        UndecidableInstances,
        OverlappingInstances
  #-}
{-
 -      ``Triple.hs''
 -      (c) 2008 James Cook
 -}

module Triple where

import Control.Monad

class Triple f where
        eta     :: a -> f a
        etaInv  :: f a -> Maybe a
        mu      :: f (f a) -> f a

instance (Functor t, Triple t) => Monad t where
        x >>= f         = (mu . fmap f) x
        return = eta

instance (Monad f) => Triple f where
        eta = return
        etaInv = const Nothing
        mu = join

