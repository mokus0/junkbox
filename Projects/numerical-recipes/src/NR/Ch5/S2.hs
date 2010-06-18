{-# LANGUAGE ViewPatterns #-}
module NR.Ch5.S2 
    ( CF, cf, gcf, asCF, asGCF
    , expand, expandFrac
    , evalCF
    
    , expandInPlace, expandInPlaceCD
    ) where

import Math.Converge
import Math.ContinuedFraction
import Data.Ratio
import Control.Arrow


evalCF :: (Fractional a, Ord a, Real a1) => CF a1 -> a -> a
evalCF cf eps = head (convergeBy (~=) (drop 2 (expand cf)))
    where
        x ~= y      = (abs (x-y) < eps)

expand :: (Real a, Fractional b) => CF a -> [b]
expand cf = convergents (fmap realToFrac cf)

expandFrac :: (Fractional a) => CF a -> [a]
expandFrac cf = convergents cf

expandInPlace :: (Fractional b, Real a) => CF a -> b
expandInPlace cf = converge (expand cf)

expandInPlaceCD :: (Fractional a, Ord a) => a -> a -> CF a -> a
expandInPlaceCD tiny eps cf
    = head (convergeBy (~=) (concatMap (drop 2) (modifiedLentz tiny cf)))
    where
        x ~= y      = (abs (x-y) < eps)
