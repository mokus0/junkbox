{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts 
  #-}
-- |CDF of Kolmogorov's D-statistic, parameterized by sample size
module Data.Random.Distribution.Kolmogorov (D(..), kCdf, kCdfQuick)where

import NR.Ch1.S4
import NR.Ch6.S1
import NR.Ch9.S1
import NR.Ch9.S3

import Data.Random

data D a = D Int
    deriving (Eq, Show)

instance Distribution D Double where
    rvar (D n) = do
        u <- stdUniform
        let f x = kCdfQuick n x - u
            (x0,x1) = last (zbrac f 0.01 0.1)
        
        case findRoot f x0 x1 eps :: Either (Brent Double Double) (Brent Double Double) of
            Left stopped    -> fail ("Failed to find root, final state was: " ++ show stopped)
            Right root      -> do
                let z = estimateRoot root
                    dz = 0.5 * estimateError root
                
                -- Very slight "blur" to account for the error in the root.
                -- Center the blur above or below the reported root depending
                -- on whether the root is above or below the zero.
                case f z `compare` 0 of
                    GT -> normal (z-dz) dz
                    EQ -> normal z dz
                    LT -> normal (z+dz) dz
                
                

instance CDF D Double where
    cdf (D n) = kCdfQuick n

shiftPointU :: Num a => a
shiftPointU = 2 ^ 450
shiftPointL :: Fractional a => a
shiftPointL = 2 ^^ (-450)

shiftDist :: Integral a => a
shiftDist = 450

shiftBase :: Num a => a
shiftBase = 2

-- Shifting and unshifting operations: These manage the external exponent
-- for a value.
shift x = shiftBy id (*) x
shiftBy f (*) (x,e)
    | fx < shiftPointL  = (shiftPointU * x, e - shiftDist)
    | fx > shiftPointU  = (shiftPointL * x, e + shiftDist)
    | otherwise         = (x,e)
    where fx = f x

unshift (x,0) = x
unshift (x,e)
    | e >= shiftDist        = unshift (shiftPointU * x, e - shiftDist)
    | e <= negate shiftDist = unshift (shiftPointL * x, e + shiftDist)
    | otherwise             = x * shiftBase ** realToFrac e

multAndUnshift (a,aE) (b,bE) = unshift (a*b, aE+bE)

-- Decompose d into k and h as described in the paper
kDecomp n d = (k, h)
    where
        dn = d * fromIntegral n
        k = ceiling dn
        h = (fromIntegral k - dn)

-- Compute the scale factor n!/n^n in "shifted" form
kScale n = foldl f (1,0) [1..n]
        where
            f (s,e) i = shift (s * fromIntegral i / fromIntegral n, e)

-- H matrix used in kCdf and k value
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
kCdf n d 
    | d <= 0    = 0
    | otherwise = multAndUnshift (kScale n) (indexM hN (k-1) (k-1), expShift)
    where
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
mPower :: (Matrix m a, Floating a, Ord a) => m a -> Int -> (m a, Int)
mPower m 1 = (m, 0)
mPower m n
    | even n    = square (mPower m (n `div` 2))
    | otherwise = m `mult` mPower m (n-1)
    where
        k = min (matRows m) (matCols m) `div` 2
        
        square (m, e) = m `mult` (m,e + e)
        mult m1 (m2, e2) = shiftBy (\m -> indexM m k k) scale (multiply m1 m2, e2)


-- Analytic limiting form (as n -> ∞)
-- kCdfLim d = sqrt (2 * pi) * recip_x * sum [exp (negate ((2 * fromIntegral i - 1)^2) * pi_2 * 0.125 * recip_x*recip_x) | i <- [1..]]
--     where
--         recip_x = recip x
--         pi_2 = pi*pi

