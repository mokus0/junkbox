module Experiments.Newtype where

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Data.List (foldl')
import System.Mem.performGC

-- At the value level, these have the same semantics.  Do they have the
-- same low-level representations?
--
-- One major difference is Bar doesn't seem to create _ANY_ symbols in the
-- generated asm.
-- 
-- Criterion tests of packing and unpacking these show Foo to be about 10%
-- slower than Bar.  For practical purposes, this is pretty much negligible.
-- 
-- Now to figure out if there's a difference in memory footprint...
data    Foo x = Foo { unFoo :: !x }
newtype Bar x = Bar { unBar ::  x }

-- what about this one?
data    Qux x = Qux { unQux ::  {-# UNPACK #-} !x }

{-# NOINLINE demand #-}
demand :: (a -> Int) -> [a] -> IO ()
demand unwrap xs = do
    evaluate (foldl' (+) 0 (map unwrap xs))
    return ()

test wrap unwrap = do
    let xs = map wrap [1..10000000]
    
    putStrLn "evaluating..."
    
    demand unwrap xs
    
    putStrLn "forcing GC"
    performGC
    putStrLn "Waiting 15 sec... "
    threadDelay (15 * 1000 * 1000)
    
    demand unwrap xs
    
    putStrLn "released"

main = test Foo unFoo

-- memory usages (live bytes after idle GCs while waiting
-- on threadDelay) (GHC 7.0.2 64-bit):
-- 
--                          -O0     -O2
-- main = test Foo unFoo
-- main = test Bar unBar
-- main = test Qux unQux
-- 
