module Benchmarks.Erf where

import Criterion.Main
import Data.Number.Erf
import Data.Random
import Data.Random.Source.DevRandom

main =  do 
    x <- sampleFrom DevRandom stdNormal :: IO Double
    putStrLn ("x = " ++ show x)
    defaultMain
        [ bench "erf"       (nf erf     x)
        , bench "erfc"      (nf erfc    x)
        , bench "erfcx"     (nf erfcx   x)
        , bench "normcdf"   (nf normcdf x)
        ]