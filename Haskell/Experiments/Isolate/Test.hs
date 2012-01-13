{-# LANGUAGE GADTs, TemplateHaskell #-}
module Experiments.Isolate.Test where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Numbers.Primes
import Experiments.Isolate
import qualified Data.Map as M
import Data.GADT.Compare.TH

data TestChan t where
    Primes      :: TestChan Integer
    Fibs        :: TestChan Integer
    Interleaved :: TestChan Integer
    StdOut      :: TestChan String

deriveGEq      ''TestChan
deriveGCompare ''TestChan

genPrimes chan = sequence_
    [ do
        threadDelay (fromInteger p * 100)
        writeChan chan p
    | p <- takeWhile (<100000) primes
    ]

genFibs chan = sequence_
    [ do
        threadDelay (fromInteger f * 100)
        writeChan chan f
    | let fibs = 0 : scanl (+) 1 fibs
    , f <- takeWhile (<100000) fibs
    ]

formatInts ints out = forever (writeChan out =<< show <$> readChan ints)

interleave xs ys out = do
    forkIO $ forever $ readChan xs >>= writeChan out
    forever          $ readChan ys >>= writeChan out

printStuff chan = forever (readChan chan >>= putStrLn)

-- WATCH OUT : with multiple chans, the 'with' combinators feed them in
-- the opposite order from what would actually "make sense".  If all your
-- channels are the same type, it's a very easy thing to miss.
test :: M.Map String (Proc TestChan (IO ()))
test = M.fromList
    [ ("genPrimes",  withChan Primes (proc genPrimes))
    , ("genFibs",    withChan Fibs (proc genFibs))
    , ("printStuff", withChan StdOut (proc printStuff))
    , ("interleave", withChan Interleaved (withChan Fibs (withChan Primes (proc interleave))))
    , ("formatInts", withChan StdOut (withChan Interleaved (proc formatInts)))
    ]

main = run test