{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Monads.Conway2 where

import Control.Applicative
import Control.Monad

data ConwayT m a
    = Pure a
    | ConwayT
        { runLeftConwayT  :: m (ConwayT m a)
        , runRightConwayT :: m (ConwayT m a)
        } 

deriving instance (Eq   a, Eq   (m (ConwayT m a))) => Eq   (ConwayT m a)
deriving instance (Ord  a, Ord  (m (ConwayT m a))) => Ord  (ConwayT m a)
deriving instance (Read a, Read (m (ConwayT m a))) => Read (ConwayT m a)
deriving instance (Show a, Show (m (ConwayT m a))) => Show (ConwayT m a)

instance Functor m => Functor (ConwayT m) where
    fmap f (Pure a) = Pure (f a)
    fmap f (ConwayT l r) = ConwayT (fmap (fmap f) l) (fmap (fmap f) return)

instance Functor m => Applicative (ConwayT m) where
    pure = Pure
    (<*>) = ap

instance Functor m => Monad (ConwayT m) where
    return = Pure
    Pure x >>= f  = f x
    ConwayT l r >>= f    = ConwayT (fmap (>>= f) l) (fmap (>>= f) r)

