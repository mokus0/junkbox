{-
 -      ``primes3.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Primes3 where

merge (x:xs) (y:ys) = case x `compare` y of
        LT      -> x : merge    xs (y:ys)
        EQ      -> x : merge    xs    ys
        GT      -> y : merge (x:xs)   ys

diff [] ys = []
diff xs [] = xs
diff (x:xs) (y:ys) = case x `compare` y of
        LT      -> x : diff    xs (y:ys)
        EQ      ->     diff    xs    ys
        GT      ->     diff (x:xs)   ys

multiples n = [n^2, n^2 + n ..]

primes = sieve (2 : 3 : 5 : scanl (+) 7 (cycle [4,2,4,2,4,6,2,6]))

sieve (x:xs) = x : sieve (diff xs (multiples x))
