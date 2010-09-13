module Math.Primes3 where

-- |Union of 2 infinite sets, represented as infinite ordered lists
-- without duplicate elements
merge (x:xs) (y:ys) = case x `compare` y of
        LT      -> x : merge    xs (y:ys)
        EQ      -> x : merge    xs    ys
        GT      -> y : merge (x:xs)   ys

-- |Difference of 2 infinite sets, represented as infinite ordered lists
-- without duplicate elements
diff [] ys = []
diff xs [] = xs
diff (x:xs) (y:ys) = case x `compare` y of
        LT      -> x : diff    xs (y:ys)
        EQ      ->     diff    xs    ys
        GT      ->     diff (x:xs)   ys

-- |Produce an infinite set of multiples of a given number, starting with its
-- square.  The set is represented as an infinite ordered list with no 
-- duplicates (n must be greater than zero).
multiples n = [n^2, n^2 + n ..]

-- |Prime numbers via the sieve of Eratosthenes, using a small \"wheel\"
-- to strike off some numbers a priori.
primes = sieve (2 : 3 : 5 : scanl (+) 7 (cycle [4,2,4,2,4,6,2,6]))

-- |The sieve of eratosthenes:  Given an infinite input set, takes the least
-- element, strikes off all multiples from the rest of the set, and sieves
-- that set.
sieve (x:xs) = x : sieve (diff xs (multiples x))
