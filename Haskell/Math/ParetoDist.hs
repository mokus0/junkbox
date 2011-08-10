{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Math.ParetoDist where

import Data.List
import Data.Random
import Math.Root.Finder
import Math.Root.Finder.Brent

data Pareto a = Pareto !a !a

instance (Floating a, Distribution StdUniform a) => Distribution Pareto a where
    rvarT (Pareto xM a) = do
        u <- stdUniformT
        return (xM / u ** recip a)

instance (Real a, Distribution Pareto a) => CDF Pareto a where
    cdf (Pareto xM a) x = 1 - (realToFrac xM / realToFrac x) ** realToFrac a

-- MLE for pareto distribution parameters
estimateParams xs = (xM, a)
    where
        n = genericLength xs
        xM = minimum xs
        a = n / sum [log x - log xM | x <- xs]

bondilandMarketFn p y0 r x = p * r / log alpha * alpha ** (x/r - 1)
    where alpha = p / y0

-- domain (0,inf)
-- range (-2375.3741647054617, 0)
bondilandMixedMarketFn x
    = bondilandMarketFn 2    90    250 x
    + bondilandMarketFn 0.01 0.10 30000 x

bondilandCDF x = 1 + bondilandMixedMarketFn x / abs (bondilandMixedMarketFn 0)

bondiland :: RVar Double
bondiland = fmap invCDF stdUniform
    where
        eps = 1e-3
        invCDF y
            = either estimateRoot id
            $ brent (\x -> bondilandCDF x - y) 0 1e10 eps

newMarketFn xM a x
    -- = (cdf (Pareto xM a) - 1) * abs (bondilandMixedMarketFn 0)
    -- = negate (realToFrac xM / realToFrac x) ** realToFrac a * 2375.3741647054617
    = negate (xM ** a * 2375.3741647054617) / x ** a
