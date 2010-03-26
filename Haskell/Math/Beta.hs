{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleContexts,
        BangPatterns
  #-}
module Math.Beta where

import Data.Random
import Data.Ratio

-- X + a ~ Gamma(a)
gammaShifted
    :: (Floating a, Ord a,
        Distribution StdUniform a, 
        Distribution Normal a)
    => a -> RVar a
gammaShifted a
    | a < 1     = do
        u <- stdUniform
        x <- gammaShifted (1+a)
        return ((x + a) * u ** recip a - a)
    | otherwise = go
    where
        !d = a - fromRational (1%3)
        !c = recip (sqrt (9*d))
        
        go = do
            x <- stdNormal
            let !cx  = c*x
                !v   = 1 + cx
            
            if v <= 0
                then go
                else do
                    u  <- stdUniform
                    let !x_2 = x*x; !x_4 = x_2*x_2
                        v3 = v*v*v
                        dv = d * v3
                    if      u < 1 - 0.0331*x_4
                     || log u < 0.5 * x_2 + d - dv + d*log v3
                        then return (d * sum (zipWith (*) [1,3,3] (iterate (*cx) 1)) - 1/3)
                        else go

-- precondition: amb + b == a
-- X + 1/2 ~ Beta(a,b)
betaShifted a b amb = do
    x <- gammaShifted a
    y <- gammaShifted b
    return (0.5 * (amb + (x - y)) / (a+b+x+y))