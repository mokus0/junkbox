{-# LANGUAGE PostfixOperators #-}
module Math.UnimodalDiscreteVariate where

import Data.Random hiding (binomial)

-- from http://cg.scs.carleton.ca/~luc/chapter_ten.pdf
-- page 495

-- "Universal rejection algorithm for unimodal distributions"
--  m is location of the mode (or any real number between adjacent modes)
--  mm is an upper bound for @prob@ at the mode
--  s_2 is an upper bound for the 2nd moment about m.  If variance and mean are
--      known, this can be given by @s_2=variance+(m-mean)^2@

-- Expected number of iterations is @mm + 3*p@.
unimodal m mm s_2 prob = go
    where
        p = (3*s_2*mm*mm)**(1/3) -- p = (3*s^2)**(1/3)*M**(2/3)
        go = do
            u <- stdUniform
            w <- stdUniform
            v <- uniform (-1) 1
            if u < p / (3*p + mm)
                then do
                    let y = m + (0.5 + p / (mm * sqrt (abs v))) * signum v
                        x = round y
                        t = w*mm*(abs v)**(3/2)
                    cont t x
                else do
                    let y = m + (0.5 + p / mm) * v
                        x = round y
                        t = w*mm
                    cont t x
        cont t x    | t <= prob x   = return x
                    | otherwise     = go

binomial n p = unimodal mode mm s_2 prob
    where
        n'      = fromIntegral n
        mean    = n' * p
        var     = mean*(1-p)
        mode    = fromIntegral (floor ((n'+1) * p))
        mm      = prob mode {- ? -}
        s_2     = var + min 1 mean
        prob k  = (n `c` k) * p^k * (1-p)^(n-k)
        
n `c` k = product [fromIntegral i | i <- [k+1..n]] / ((n-k)!)
(!) n   = product [fromIntegral i | i <- [1..n]]