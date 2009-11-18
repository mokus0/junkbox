{-
 -      ``IOBoolMonoid''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        FlexibleInstances
  #-}

module TypeExperiments.IOBoolMonoid where

import Data.Monoid

instance Monoid (IO Bool) where
        mempty = return True
        mappend x y = do
                x <- x
                if x 
                        then y
                        else return False