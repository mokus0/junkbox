{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, UndecidableInstances
  #-}
module Math.Gamma where

import Data.Random
import qualified NR.Ch6.S1 as NR

instance (Distribution Gamma a, Real a) => CDF Gamma a where
    cdf (Gamma k theta) x = exp (undefined k (x / theta) - NR.gammln (realToFrac k))
