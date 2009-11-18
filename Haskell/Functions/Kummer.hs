{-
 -      ``Kummer''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        ParallelListComp
  #-}

module Kummer where

import ConvergingSum

m a b z = convergingSum 0
    [ (a `rfp` n) * z ^ n / ((b `rfp`n) * n_fac)
    | n <- [0..]
    | n_fac <- scanl (*) 1 [1..]
    ]

rfp a n = product [a + fromIntegral i | i <- [0..n-1]]

epsilon x y = 1e-10 * min x y

x ~= y = abs (x-y) < epsilon x y