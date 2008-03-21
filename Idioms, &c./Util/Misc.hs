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

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

fix :: (a -> a) -> a
fix f = f (fix f)

infixl 8 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy (*) x y = case x * y of
        LT      -> y
        _       -> x

waitForEvent p events = waitForEvent' p (readTChan events)

waitForEvent' p events = do
        event <- events
        if p event
                then return event
                else retry

waitFor p = do
        x <- p
        if x
                then return ()
                else retry

atomLoop :: STM a -> IO ()
atomLoop = forever . atomically

forkLoop :: STM a -> IO ThreadId
forkLoop = forkIO . atomLoop

toHex :: Word8 -> String
toHex = printf "%02x"


hexdump :: [Word8] -> String
hexdump str = intercalate " " (map toHex str)