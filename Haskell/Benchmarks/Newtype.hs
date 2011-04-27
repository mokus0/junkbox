module Benchmarks.Newtype where

import Criterion.Main
import Experiments.Newtype

main = defaultMain
    [ bench "return ()" (return () :: IO ())
    -- Interesting; ^ this takes 25 ns with -O0 and 8 ns with -O2, whereas
    -- the rest are unchanged.  Which tells me there's about a 15 ns overhead
    -- just for using 'nf'.  (particular numbers may vary, obviously)
    , bench "id"  (nf id ())
    , bench "foo" (nf unFoo (Foo ()))
    , bench "bar" (nf unBar (Bar ()))
    , bench "qux" (nf unQux (Qux ()))
    
    -- also, I was curious about whether any evaluation is done on the 
    -- result of an IO test.  It is not - this one runs in the same time as 
    -- "return ()"
    , bench "return ⊥" (return (let x = x in x) :: IO ())
    ]