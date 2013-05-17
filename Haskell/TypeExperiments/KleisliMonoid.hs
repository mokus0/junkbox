{-# LANGUAGE FlexibleInstances #-}
module TypeExperiments.KleisliMonoid where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Traversable as T

instance (Monad m, MonadPlus f, Traversable f) => Monoid (Kleisli m a (f b)) where
    mempty = Kleisli (const (return mzero))
    mappend (Kleisli f) (Kleisli g) = Kleisli $ \a -> do
        fb <- f a
        liftM join (T.sequence (fmap (return . return) fb `mplus` return (g a)))
