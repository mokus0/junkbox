#!/usr/bin/env runhaskell
module Main where

import M1M2
import Criterion.Main

main = defaultMain
    [ bench "m1" (nf m1 1000000)
    , bench "m2" (nf (m2 :: Int -> Integer) 1000000)
    , bench "m3" (nf m3 1000000)
    ]
