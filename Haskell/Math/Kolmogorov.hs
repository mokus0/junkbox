{-# LANGUAGE FlexibleContexts #-}
module Math.Kolmogorov where

import NR.Ch1.S4

-- CDF of Kolmogorov's D-statistic

-- kDecomp :: (Integral a) => a -> b -> (a, b)
kDecomp n d = (k, h)
    where
        dn = d * fromIntegral n
        k = ceiling dn
        h = (fromIntegral k - dn)

kCdf n d = scale * indexM (matExp hMat n) (k-1) (k-1)
    where
        umatrix :: (Matrix UMatrix t) => Int -> Int -> (Int -> Int -> t) -> UMatrix t
        umatrix = matrix
        
        scale = fromInteger (product [1..toInteger n]) / fromInteger (toInteger n^n)
        
        hMat :: UMatrix Double
        hMat = matrix m m hMatCell
        
        spine n = (1 - h^n) / product [1..fromIntegral n]
        
        hMatCell r 0 | r == m-1     = (1 - 2 * h ^ m + max 0 ((2 * h - 1)^m)) / product [1..fromIntegral m]
        hMatCell r 0                = spine (fromIntegral r + 1)
        hMatCell r c | r == m-1     = spine (m - fromIntegral c)
        hMatCell r c
            | i - j + 1 >= 0    = 1 / fromIntegral (product [1 .. (i-j+1)])
            | otherwise         = 0
            where i = r + 1; j = c + 1
        
        (k, h) = kDecomp n d
        m = 2 * k - 1

matExp :: (Matrix m a, Num a) => m a -> Int -> m a
matExp m 1 = m
matExp m n
    | even n    = square (matExp m (n `div` 2))
    | otherwise = m `multiply` matExp m (n-1)
    where
        square m = m `multiply` m
        

-- kCdfLim d = sqrt (2 * pi) * recip_x * sum [exp (negate ((2 * fromIntegral i - 1)^2) * pi_2 * 0.125 * recip_x*recip_x) | i <- [1..]]
--     where
--         recip_x = recip x
--         pi_2 = pi*pi