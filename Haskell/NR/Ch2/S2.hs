{-
 - Purely functional (and lazy) forward- and back-substitution
 - (solving of lower and upper triangular systems, respectively).
 -
 - NB - it is possible to generalize the types to cover unboxed
 - outputs, but DO NOT DO SO!  The laziness of the output is a
 - critical part of the algorithms.
 -}
module NR.Ch2.S2 where

-- TODO: proper size checking
import NR.Ch1.S4
import Data.List

backSub :: (Fractional t, Matrix m t, Vector v t)
    => m t -> v t -> IVector t
backSub a b = x
    where
        n = matCols a
        x = vector n $ \i ->
            (indexV b i - sum [indexM a i k * indexV x k | k <- [i+1 .. n-1]]) / indexM a i i
        
        sum [] = 0
        sum xs = foldl1' (+) xs

backSubs :: (Fractional t, Matrix m t, Matrix v t)
    => m t -> v t -> IMatrix t
backSubs a b = x
    where
        n = matCols a
        bs = matCols b
        x = matrix n bs $ \i j ->
            (indexM b i j - sum [indexM a i k * indexM x k j | k <- [i+1 .. n-1]]) / indexM a i i
        
        sum [] = 0
        sum xs = foldl1' (+) xs

forwardSub :: (Fractional t, Matrix m t, Vector v t)
    => m t -> v t -> IVector t
forwardSub a b = x
    where
        n = matCols a
        x = vector n $ \i ->
            (indexV b i - sum [indexM a i k * indexV x k | k <- [0 .. i-1]]) / indexM a i i
        
        sum [] = 0
        sum xs = foldl1' (+) xs

forwardSubs :: (Fractional t, Matrix m t, Matrix v t)
    => m t -> v t -> IMatrix t
forwardSubs a b = x
    where
        n = matCols a
        bs = matCols b
        x = matrix n bs $ \i j ->
            (indexM b i j - sum [indexM a i k * indexM x k j | k <- [0 .. i-1]]) / indexM a i i
        
        sum [] = 0
        sum xs = foldl1' (+) xs

