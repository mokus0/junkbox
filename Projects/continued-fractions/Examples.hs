module Examples where

import Math.ContinuedFraction

-- |Euler's formula for computing @sum (map product (inits xs))@.
sumPartialProducts :: Num a => [a] -> CF a
sumPartialProducts (x:xs) = gcf 0 ((x,1):[(negate x, 1 + x) | x <- xs]++[])