{-
 -      ``Update''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    UndecidableInstances
  #-}

module TypeExperiments.Update where

import Data.StateRef
import Data.Array.MArray

class Update a u | u -> a where
    mkUpdate :: (a -> a) -> u

instance Update a (a -> a) where
    mkUpdate = id

instance ModifyRef sr m a => Update a (sr -> m ()) where
    mkUpdate = flip modifyRef

instance (MArray a e m, Ix i) => Update e (i -> a i e -> m ()) where
    mkUpdate f i a = do
        x <- readArray a i
        writeArray a i (f x)