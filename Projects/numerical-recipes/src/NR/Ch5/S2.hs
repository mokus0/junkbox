{-# LANGUAGE ViewPatterns #-}
module NR.Ch5.S2 
    ( CF, cf, gcf, asCF, asGCF
    , expand, expandFrac
    , evalCF
    
    , expandInPlace, expandInPlaceCD
    ) where

import Math.Sequence.Converge
import Math.ContinuedFraction
import Data.Either
import Data.Maybe
import Data.Ratio
import Control.Arrow


evalCF :: (Fractional a, Ord a, Real a1) => CF a1 -> a -> a
evalCF cf eps = fromMaybe empty (convergeBy eq Just (expand cf))
    where
        empty = error "evalCF: programming error, CF had no convergents!"
        eq (_:_:x:y:_)
            | abs (x-y) <= abs eps      = Just y
            | otherwise                 = Nothing

expand :: (Real a, Fractional b) => CF a -> [b]
expand cf = convergents (fmap realToFrac cf)

expandFrac :: (Fractional a) => CF a -> [a]
expandFrac cf = convergents cf

expandInPlace :: (Fractional b, Real a) => CF a -> b
expandInPlace cf = converge (expand cf)

expandInPlaceCD :: (Fractional a, Ord a) => a -> a -> CF a -> a
expandInPlaceCD tiny eps cf
    = fromMaybe empty 
    . convergeBy (listToMaybe . rights) (either Just Just)
    . catMaybes
    . map (convergeBy eq (Just . Left))
    $ modifiedLentz tiny cf
    where
        empty = error "evalCF: programming error, CF had no convergents!"
        eq (a:b:x:y:_)
            | b /= b                    = Just (Right a)
            | abs (x-y) <= abs eps      = Just (Right y)
            | otherwise                 = Nothing
