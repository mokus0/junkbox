-- |List block-memoization helpers
-- 
-- Example usage:
-- 
-- > fib 0 = 0
-- > fib 1 = 1
-- > fib n = fib (n-2) + fib (n-1)
-- >     where fib = unblock fibs
-- >
-- > fibs = block 0 squares (map fib [0..])
-- > squares = scanl1 (+) [1,3..]
module Block where

import Data.Array
import Control.Monad

-- |Given a starting logical offset, a list of chunk sizes, and a list of values,
-- pack the values into several arrays of the given sizes, with array indices
-- corresponding to the logical offset of the element in the value list.
block :: (Integral i, Ix i) => i -> [i] -> [e] -> [Array i e]
block _ _ [] = []
block start (sz:szs) list = case splitAt (fromIntegral sz + 1) list of
        (pre, post) -> listArray (start, start + sz) pre
                        : block (start + sz + 1) szs post

-- |Look up a logical offset in a list of arrays returned by 'block'.
unblock :: (Num i, Ix i) => [Array i e] -> i -> e
unblock arrs i = head 
    [ arr ! i
    | arr <- arrs
    , inRange (bounds arr) i
    ]
