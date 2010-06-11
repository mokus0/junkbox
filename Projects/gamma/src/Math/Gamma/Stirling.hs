{-# LANGUAGE ParallelListComp #-}
-- |Stirling's approximation to the gamma function
module Math.Gamma.Stirling (lnGammaStirling, cs, s, abs_s) where

-- |Convergent when Re(z) > 0.  The first argument is the c_n series to use 
-- ('cs' is an ineffecient but generic definition of the full infinite series.
-- Some precomputed finite prefix of 'cs' should be fed to this function, the 
-- length of which will determine the accuracy achieved.)
lnGammaStirling cs z = (z - 0.5) * log z - z + 0.5 * log (2*pi) + sum [c / q | c <- cs | q <- qs z]
    where
        qs z = map (q z) [1..]
        q z n = product (take n (iterate (+1) (z+1)))

-- |The c_n series in the convergent version of stirling's approximation given
-- on wikipedia at
-- http://en.wikipedia.org/wiki/Stirling%27s_approximation#A_convergent_version_of_Stirling.27s_formula
-- as fetched on 11 June 2010.
cs :: Fractional a => [a]
cs = map c [1..]

c :: Fractional a => Int -> a
c n = 0.5 * recip n' * sum [k' * fromIntegral (abs_s n k) / ((k' + 1) * (k' + 2)) | k <- [1..n], let k' = fromIntegral k]
    where n' = fromIntegral (n :: Int)

-- |The (signed) Stirling numbers of the first kind.
-- TODO: memoize or implement some closed form (or both)
s 0 0 = 1
s _ 0 = 0
s n k
    | n < 0     = error "s n k: n < 0"
    | k < 0     = error "s n k: k < 0"
    | k == n    = 1
    | k > n     = error "s n k: k > n"
    | otherwise = s (n-1) (k-1) - (n-1) * s (n-1) k

-- |The (unsigned) Stirling numbers of the first kind.
-- TODO: memoize or implement some closed form (or both)
abs_s 0 0 = 1
abs_s _ 0 = 0
abs_s n k
    | n < 0     = error "abs_s n k: n < 0"
    | k < 0     = error "abs_s n k: k < 0"
    | k == n    = 1
    | k > n     = error "abs_s n k: k > n"
    | otherwise = abs_s (n-1) (k-1) + (n-1) * abs_s (n-1) k

