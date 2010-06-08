{-# LANGUAGE FlexibleContexts #-}
module Math.Kolmogorov where

import NR.Ch1.S4
import NR.Ch6.S1

kDecomp n d = (k, h)
    where
        dn = d * fromIntegral n
        k = ceiling dn
        h = (fromIntegral k - dn)

-- H matrix used in kCdf
kCdfMat
  :: (RealFrac t, Enum t, Matrix UMatrix t, Integral a, Factorial t) =>
     a -> t -> (UMatrix t, Int)
kCdfMat n d = (matrix m m hMatCell, k)
    where
        spine n = (1 - h^n) / factorial n
        
        hMatCell r 0 | r == m-1     = (1 - 2 * h ^ m + max 0 ((2 * h - 1)^m)) / factorial m
        hMatCell r 0                = spine (fromIntegral r + 1)
        hMatCell r c | r == m-1     = spine (m - fromIntegral c)
        hMatCell r c
            | i - j + 1 >= 0    = 1 / factorial (fromIntegral (i-j+1))
            | otherwise         = 0
            where i = r + 1; j = c + 1
        
        (k, h) = kDecomp n d
        m = 2 * k - 1

-- CDF of Kolmogorov's D-statistic for a given sample size n
kCdf n d = scale * indexM hN (k-1) (k-1)
    where
        n' = fromIntegral n
        logScale = factln (toInteger n) - n' * log n' -- fromInteger (factorial (toInteger n)) / fromInteger (toInteger n^n)
        scale = exp (logScale + expShift)
        (hN, expShift) = mPower hMat n
        (hMat, k) = kCdfMat n d

-- |'kCdf' with Marsaglia's long-computation shortcut approximation.
-- Accurate to about 7 decimal places in the right tail of the distribution.
kCdfQuick n d
    | s > 7.24 || (s > 3.76 && n > 99)
        = 1 - 2 * exp (negate (2.000071 + 0.331/sqrt n' + 1.409/n') * s)
    | otherwise = kCdf n d
    where s = d*d*n'; n' = fromIntegral n

-- |Matrix power to Int exponent with explicitly managed exponent.  Returns
-- a multiple of m^n, along with the natural logarithm of the factor.
--
-- That is, if @(mn, logS) = mPower m n@ then m^n = (e^logS) * mn
mPower :: (Matrix m a, Floating a, Ord a) => m a -> Int -> (m a, a)
mPower m 1 = (m, 0)
mPower m n
    | even n    = square (mPower m (n `div` 2))
    | otherwise = m `mult` mPower m (n-1)
    where
        k = min (matRows m) (matCols m) `div` 2
        
        square (m, e) = m `mult` (m,e + e)
        mult m1 (m2, e2) 
            | pivot > exp 300 = (m4,e4)
            | otherwise     = (m3,e3)
            where
                m3 = multiply m1 m2
                e3 = e2
                pivot = indexM m3 k k
                m4 = liftLinear (* exp (-300)) m3
                e4 = e3 + 300
                
        

-- kCdfLim d = sqrt (2 * pi) * recip_x * sum [exp (negate ((2 * fromIntegral i - 1)^2) * pi_2 * 0.125 * recip_x*recip_x) | i <- [1..]]
--     where
--         recip_x = recip x
--         pi_2 = pi*pi

