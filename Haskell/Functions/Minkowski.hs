{-# LANGUAGE ViewPatterns #-}
module Functions.Minkowski where

import Data.Ratio

(?) x = go 100 3 (minkowski (toRational x))
    where
        -- j is a timeout parameter.
        -- n here is an "overconvergence" parameter.  It's the number of times
        -- it has to see a small enough eps before it accepts the output.
        go j n ((x, dx):rest)
            | j <= 0 || n <= 0  = x'
            | x' + dx' == x'    = go (j-1) (n-1) rest
            | otherwise         = go (j-1)  n    rest
            where 
                x'  = fromRational x
                dx' = fromRational dx

minkowski :: Rational -> [(Rational, Rational)]
minkowski x = let p = floor x :: Integer in go p 1 (p+1) 1 1 (fromInteger p)
    where
        go p q r s ((* 0.5) -> d) y 
            | x < m % n             = (y, d) : go p q m n d y
            | otherwise             = (y, d) : go m n r s d (y+d)
            where
                m = p + r
                n = q + s
    
    