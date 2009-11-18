{-
 -      ``Data/ToString''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances,
        UndecidableInstances,
        OverlappingInstances
  #-}

module Data.ToString where

import Data.StateRef

class Monad m => ToString m a where
        toString :: a -> m String

instance (Show a, ReadRef sr m a) => ToString m sr
        where toString = (>>= (return . show)) . readRef
