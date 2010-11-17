{-# LANGUAGE FlexibleContexts #-}
module Functions.IsPermutation where

import qualified Data.Vector as V
import Data.List (sort)

test :: [Int] -> Bool
test xs = isPerm xVec == isPerm' xVec
    where xVec = V.fromList xs

-- input: immutable array of N integers in range 1 to N, inclusive.  Output:  
-- True if the list is a permutation of [1..N], False otherwise.
-- Goal is O(n) time and O(1) space.
-- reference implementation(O(n*log(n)) time, O(n) space, I believe):
isPerm' :: V.Vector Int -> Bool
isPerm' xs = sort xList == zipWith const [1..] xList
    where xList = V.toList xs

-- This one is O(n^2) time and O(1) space, I think...
-- The worst-case time is achieved in the case where the input list is a 
-- permutation with a single cycle of length n.
isPerm :: V.Vector Int -> Bool
isPerm xs = and
    [ i `elem` tail (take n (traceFrom i xs))
    | i <- [1..n]
    ]
    where
        n = V.length xs

traceFrom  :: Int -> V.Vector Int -> [Int]
traceFrom n vec = iterate next n
    where
        next n = vec V.! (n-1)

-- Find a cycle in an (infinite) list in O(length) time and O(1) space
-- (ideally - not actually, unless the list is truly free; if it's lazy,
-- this has a major potential space leak)
floyd :: Eq a => [a] -> Int
floyd xs = go 1 (drop 1 xs) (drop 2 xs)
    where
        go n (x:xs) (y:ys)
            | x == y        = n
            | otherwise     = go (n+1) (drop 1 xs) (drop 2 ys)
        go _ _ _ = error "floyd: sequence is finite"

tailLength :: Eq a => Int -> [a] -> Int
tailLength cycleLength xs = go 0 (drop cycleLength xs) xs
    where
        go n (x:xs) (y:ys)
            | x == y        = n
            | otherwise     = go (n+1) xs ys
