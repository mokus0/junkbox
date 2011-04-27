module Benchmarks.MWC where

import Control.Monad.Primitive (RealWorld)
import Criterion.Main
import Data.Vector.Unboxed (Vector)
import System.Random.MWC

main =  do 
    src <- create
    defaultMain
        [ bench "create"             (create :: IO (Gen RealWorld))
        , bench "uniform"            (uniform src :: IO Double)
        , bench "normal"             (normal  src :: IO Double)
        , bench "uniformVector 1000" (uniformVector src 1000 :: IO (Vector Double))
        ]