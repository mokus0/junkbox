{-
 -      ``MixedMath''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies,
        FlexibleInstances
  #-}

module MixedMath where

import Prelude hiding ((+))
import qualified Prelude as P

infixl 6 +

class Add a b c | a b -> c where
        (+) :: a -> b -> c

instance Num a => Add a a a where
        (+) = (P.+)

instance Add Float Double Double where
        f + d = (realToFrac f :: Double) + d
instance Add Double Float Double where
        d + f = d + (realToFrac f :: Double)
