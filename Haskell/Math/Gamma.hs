{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, UndecidableInstances
  #-}
module Math.Gamma where

import Data.Random
import NR.Ch6.S2

instance (Distribution Gamma a, Real a) => CDF Gamma a where
    cdf (Gamma k theta) x = gammp (realToFrac k) (realToFrac x / realToFrac theta)
