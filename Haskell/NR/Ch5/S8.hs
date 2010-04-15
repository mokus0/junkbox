{-# LANGUAGE RecordWildCards, BangPatterns, FlexibleContexts #-}
-- |Fitting and evaluating Chebyshev polynomials
module NR.Ch5.S8 where

import Data.Vector.Generic (Vector, generate, (!))
import qualified Data.Vector.Generic as V

data Chebyshev v a b = Chebyshev
    { chebOrder     :: Int
    -- ^ Order of the (truncated) approximation, which may be different
    -- from the order of the fitted approximation, which is
    -- @length chebCoeffs - 1@
    , chebCoeffs    :: v b
    , chebA         :: a
    , chebB         :: a
    } deriving (Eq, Show)

chebMaxOrder Chebyshev{..} = length chebCoeffs - 1

repackCheb f Chebyshev{..} = Chebyshev{chebCoeffs = f chebCoeffs, ..}

truncateChebAt m Chebyshev{..} = Chebyshev{chebOrder = m, ..}

truncateChebWhere p cheb@Chebyshev{..} = truncateChebAt m cheb
    where
        m = case take 1 [j | j <- [chebOrder, chebOrder-1 .. 0], not (p (chebCoeffs!j))] of
                [newOrder]  -> newOrder
                []          -> chebOrder

truncateChebEps eps = truncateChebWhere (< eps)

-- |Create a chebyshev approximation to the given function,
-- of the given order, over the given interval.
-- 
-- I don't know whether it's due to instability, weirdness of GHC's 'cos',
-- or something else, but in my experimentation i cannot get this to generate
-- an erfc approximation over 0 <= x <= 1 any more accurate than about 1e-16.
{-# SPECIALIZE cheb :: Vector v Double => (Double -> Double) -> Int -> Double -> Double -> Chebyshev v Double Double #-}
cheb :: (Floating a, Floating b, Vector v b) => (a -> b) -> Int -> a -> a -> Chebyshev v a b
cheb f chebOrder chebA chebB = Chebyshev{..}
    where
        generate' = generate -- using monomorphism restriction to force choice of one vector type to use internally
        
        n           = chebOrder + 1
        bma         = 0.5 * (chebB - chebA)
        bpa         = 0.5 * (chebB + chebA)
        
        fk          = generate' n $ \k -> 
                        let y = cos (pi * (fromIntegral k + 0.5) / fromIntegral n)
                         in f (y * bma + bpa)
        
        fac         = 2 / fromIntegral n
        chebCoeffs  = generate' n $ \j ->
                        fac * V.sum (generate' n $ \k -> (fk!k) * cos (pi * fromIntegral j * (fromIntegral k + 0.5) / fromIntegral n))
        chebTruncErr = Nothing

-- |Evaluate a Chebyshev polynomial
chebEval :: (Real a, Fractional b, Vector v b) => Chebyshev v a b -> a -> b
chebEval c = chebEvalM (chebOrder c) c

-- |Evaluate the m-term truncation of a Chebyshev polynomial
chebEvalM :: (Real a, Fractional b, Vector v b) => Int -> Chebyshev v a b -> a -> b
chebEvalM order Chebyshev{..} x = go order 0 0 
    where
        go !j !dd !d
            | j > 0     = go (j-1) d (y2*d - dd + (chebCoeffs!j))
            | otherwise = y * d - dd + 0.5 * (chebCoeffs!0)
        
        y  = (2 * realToFrac x - a - b) / (b - a)
        y2 = 2 * y
        
        a = realToFrac chebA
        b = realToFrac chebB
    

chebEvalWith :: (Num a, Vector v a) => (a -> a -> a) -> Chebyshev v a a -> a -> a
chebEvalWith (/) c = chebEvalMWith (/) (chebOrder c) c
    
-- |Evaluate the m-term truncation of a Chebyshev polynomial with a user-provided
-- division operator.
chebEvalMWith :: (Num a, Vector v a) => (a -> a -> a) -> Int -> Chebyshev v a a -> a -> a
chebEvalMWith (/) order Chebyshev{..} x = go order 0 0 
    where
        go !j !dd !d
            | j > 0     = go (j-1) d (y2*d - dd + (chebCoeffs!j))
            | otherwise = y * d - dd + (chebCoeffs!0) / 2
        
        y  = (2 * x - a - b) / (b - a)
        y2 = 2 * y
        
        a = chebA
        b = chebB
    
    
