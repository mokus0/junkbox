{-
 -      ``Block''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Block where

import Data.Array
import Control.Monad

block :: (Integral i, Ix i) => i -> [i] -> [e] -> [Array i e]
block _ _ [] = []
block start (sz:szs) list = case splitAt (fromIntegral sz + 1) list of
        (pre, post) -> listArray (start, start + sz) pre
                        : block (start + sz + 1) szs post

unblock :: (Num i, Ix i) => [Array i e] -> i -> e
unblock arrs i = head $ do
        arr <- arrs
        let     bnds = bounds arr
                inBnds = inRange bnds i
        if inBnds
                then return (arr ! i)
                else mzero
