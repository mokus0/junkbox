{-
 -      ``Util/Misc.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Util.Misc where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad

import Data.Word
import Data.List
import Text.Printf

import Control.Monad.Loops

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy (*) x y = case x * y of
        LT      -> y
        _       -> x

limit :: Integral b => [a] -> b -> [a] -> [a]
limit _   _      []    = []
limit end (n+1) (x:xs) = x : limit end n xs
limit end _     (x:xs) = end

limitStr :: Integral a => a -> [Char] -> [Char]
limitStr n = limit "..." (n-3)

tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan c = fmap Just (readTChan c) `orElse` return Nothing

readAllTChan :: TChan a -> STM [a]
readAllTChan c = unfoldM (tryReadTChan c)


minimaBy :: (a -> a -> Ordering) -> [a] -> [a]
minimaBy cmp list = minimaBy' [] list
        where   minimaBy' ms []           = ms
                minimaBy' [] (x:xs)       = minimaBy' [x] xs
                minimaBy' ms@(m:_) (x:xs) = case m `cmp` x of
                        LT -> minimaBy' ms      xs
                        EQ -> minimaBy' (x:ms)  xs
                        GT -> minimaBy' [x]     xs

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)