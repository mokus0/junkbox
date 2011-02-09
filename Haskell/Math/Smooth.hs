module Math.Smooth where

import Control.Monad
import Data.List

f x | x <= 0    = 0
    | x >= 1    = 1
    | otherwise = a / (a + b)
    where 
        a = exp (- recip x)
        b = exp (- recip (1-x))

g x = 1 - f x

ease x0 x1 a = g a * x0 + f a * x1

split r x = (h f, h g)
    where
        h side t = side (0.5 + (x - t) / (2 * r))

-- Build a smooth partition of unity where every break has radius 'r' and there
-- are breaks at every point in the given list
partitions r = loop [const 1] . sortBy (flip compare)
    where
        loop fs [] = fs
        loop fs (x:xs) = loop (f:map (mul g) fs) xs
            where (f,g) = split r x
        
        mul f g x = f x * g x
