{-# LANGUAGE RecordWildCards, BangPatterns #-}
module NR.Ch5.S8 where

import Data.Vector (Vector, generate, (!))
import qualified Data.Vector as V

data Chebyshev v a b = Chebyshev
    { chebCoeffs    :: v b
    , chebA         :: a
    , chebB         :: a
    , chebTruncErr  :: b
    -- ^ estimated error due to truncation of a series 
    --   (this is separate from any approximation error arising
    --   from the use of a series in the first place)
    } deriving (Eq, Show)

chebOrder Chebyshev{..} = V.length chebCoeffs - 1

repackCheb f Chebyshev{..} = Chebyshev{chebCoeffs = f chebCoeffs, ..}
truncateCheb f Chebyshev{..} = case f chebCoeffs of
    (chebCoeffs, newErr) -> Chebyshev{chebTruncErr = chebTruncErr + newErr, ..}
truncateChebAt m = truncateCheb toM
    where
        toM v = (V.take m v, V.sum (V.map abs (V.drop m v)))
truncateChebWhere p = truncateCheb trunc
    where
        trunc v = case V.span p (V.reverse v) of
            (discard, keep) -> (V.reverse keep, V.sum (V.map abs discard))
truncateChebEps eps = truncateChebWhere (< eps)

-- |Create a chebyshev approximation to the given function,
-- of the given order, over the given interval.
-- 
-- I don't know whether it's due to instability, weirdness of GHC's 'cos',
-- or something else, but in my experimentation i cannot get this to generate
-- an erfc approximation over 0 <= x <= 1 any more accurate than about 1e-16.
{-# SPECIALIZE cheb :: (Double -> Double) -> Int -> Double -> Double -> Chebyshev Vector Double Double #-}
cheb :: (Floating a, Floating b) => (a -> b) -> Int -> a -> a -> Chebyshev Vector a b
cheb f order chebA chebB = Chebyshev{..}
    where
        n           = order + 1
        bma         = 0.5 * (chebB - chebA)
        bpa         = 0.5 * (chebB + chebA)
        
        fk          = generate n $ \k -> 
                        let y = cos (pi * (fromIntegral k + 0.5) / fromIntegral n)
                         in f (y * bma + bpa)
        
        fac         = 2 / fromIntegral n
        chebCoeffs  = generate n $ \j ->
                        fac * V.sum (generate n $ \k -> (fk!k) * cos (pi * fromIntegral j * (fromIntegral k + 0.5) / fromIntegral n))
        chebTruncErr = 0

chebEval :: (Real a, Fractional b) => Chebyshev Vector a b -> a -> b
chebEval c = chebEvalM (chebOrder c) c

chebEvalM :: (Real a, Fractional b) => Int -> Chebyshev Vector a b -> a -> b
chebEvalM order Chebyshev{..} x = go order 0 0 
    where
        go !j !dd !d
            | j > 0     = go (j-1) d (y2*d - dd + (chebCoeffs!j))
            | otherwise = y * d - dd + 0.5 * (chebCoeffs!0)
        
        y  = (2 * realToFrac x - a - b) / (b - a)
        y2 = 2 * y
        
        a = realToFrac chebA
        b = realToFrac chebB
    
    
