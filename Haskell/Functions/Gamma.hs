{-
 -      ``Gamma''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        ParallelListComp, ForeignFunctionInterface
  #-}

module Gamma where

import Kummer

lowerGamma s z = z ** s * m s (s+1) (negate z) / s
upperGamma s z = gamma s - lowerGamma s z

foreign import ccall "math.h tgamma" gamma :: Double -> Double

-- gamma x
--     | x >= 0.5
--     = sqrt (2*pi) * t**(z + 0.5) * exp (negate t) * a g z
--     
--     | otherwise
--     = (pi / sin (pi * x)) * gamma (1 - x)
--     
--     where
--         z = x - 1
--         g = 7
--         
--         t = z + g + 0.5
-- 
-- a g z = convergingSum (p 0) [p / (z + fromIntegral n) | n <- [1..] | p <- tail ps]
-- 
-- ps :: Floating a => [a]
-- ps = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
--      771.32342877765313, -176.61502916214059, 12.507343278686905,
--      -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]
-- 
-- p n | n < length ps = ps !! n
--     | otherwise     = 0
-- 
-- 
-- p_ k g = sum
--     [ c (2 * k + 1) (2 * a' + 1)
--     * sqrt 2 / pi
--     * factorial (a - 0.5)
--     * (a + g + 0.5) ** (negate (a + 0.5))
--     * exp (a + g + 0.5)
--     | a' <- [0..k]
--     , let a = fromIntegral a'
--     ]
-- 
-- factorial n = product [n, n-1 .. 1]
-- 
-- c :: Num a => Int -> Int -> a
-- c 1 1 = 1
-- c 2 2 = 1
-- c i 1 
--     | i >= 3
--     = negate (c (i-2) 1) 
-- c i j 
--     | i == j
--     = 2 * c (i - 1) (j - 1)
-- 
--     | otherwise
--     = 2 * c (i - 1) (j - 1) - c (i - 2) j