module Functions.Parts where

-- |Given a natural number n, return a list of all multisets (as ordered lists)
-- of smaller natural numbers that sum to n
parts n = partsFrom 1 n
    where
        partsFrom a 0 = [[]]
        partsFrom a n = [i:is | i <- [a..n], is <- partsFrom i (n - i)]
