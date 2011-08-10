{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Monads.Conway where

import Control.Applicative
import Control.Monad

data ConwayT m a
    = ConwayT
        { runLeftConwayT  :: m (Either a (ConwayT m a))
        , runRightConwayT :: m (Either a (ConwayT m a))
        } 

deriving instance (Eq   a, Eq   (m (Either a (ConwayT m a)))) => Eq   (ConwayT m a)
deriving instance (Ord  a, Ord  (m (Either a (ConwayT m a)))) => Ord  (ConwayT m a)
deriving instance (Read a, Read (m (Either a (ConwayT m a)))) => Read (ConwayT m a)
deriving instance (Show a, Show (m (Either a (ConwayT m a)))) => Show (ConwayT m a)

instance Functor m => Functor (ConwayT m) where
    fmap f (ConwayT l r) = ConwayT (fmap g l) (fmap g r)
        where
            g (Left  x) = Left (f x)
            g (Right x) = Right (fmap f x)

bind :: (forall a b. (a -> b) -> (f a -> f b))
     -> ConwayT f s
     -> (s -> ConwayT f t)
     -> ConwayT f t
bind liftS (ConwayT l r) f = ConwayT
    (liftS g l)
    (liftS g r)
    where
        g (Left  x) = Right (f x)
        g (Right x) = Right (bind liftS x f)

newtype L f a = L { runL :: f a } deriving (Eq, Ord, Read, Show)

instance Functor m => Functor (L (ConwayT m)) where
    fmap f (L x) = L (fmap f x)

instance MonadPlus m => Monad (L (ConwayT m)) where
    return x = L (ConwayT (return (Left x)) mzero)
    L x >>= f   = L (bind liftM x (runL . f))

newtype R f a = R { runR :: f a } deriving (Eq, Ord, Read, Show)

instance Functor m => Functor (R (ConwayT m)) where
    fmap f (R x) = R (fmap f x)

instance MonadPlus m => Monad (R (ConwayT m)) where
    return x = R (ConwayT mzero (return (Left x)))
    R x >>= f   = R (bind liftM x (runR . f))

