{-# LANGUAGE 
        MultiParamTypeClasses,
        FlexibleContexts, FlexibleInstances,
        GADTs,
        ParallelListComp
  #-}
module Math.Statistics.KSTest (ks, ksTest, KS(..)) where

import Control.Monad
import Data.Random
import Data.Random.Distribution.Kolmogorov (kCdf, kCdfQuick)
import Data.List (sort)

-- |Kolmogorov-Smirnov statistic for a set of data relative to a (continuous)
-- distribution with the given CDF.  Returns 3 common forms of the statistic:
-- (K+, K-, D), where K+ and K- are Smirnov's one-sided forms as presented in
-- Knuth's Semi-Numerical Algorithms (TAOCP, vol. 2)  and D is Kolmogorov's
-- undirected version.
-- 
-- In particular,
--
-- *   K+ = sup(\x -> F_n(x) - F(x))
-- *   K- = sup(\x -> F(x) - F_n(x))
-- *   D  = sup(\x -> abs(F_n(x) - F(x)))
--
ks f n xs = (kPlus, kMinus, d)
    where
        sqrt_n = sqrt (fromIntegral n)
        sorted = sort (map f xs)
        
        kPlus   = maximum [ j//n -  fx  | j <- [1..] | fx <- sorted]
        kMinus  = maximum [ fx   - j//n | j <- [0..] | fx <- sorted]
        d       = max kPlus kMinus
        
        infixl 7 //
        x//y = fromIntegral x / fromIntegral y

-- | @ksTest cdf xs@ 
-- Computes the probability of a random data set (of the same size as xs)
-- drawn from a continuous distribution with the given CDF having the same
-- Kolmogorov statistic as xs.
--
-- The statistic is the greatest absolute deviation of the empirical CDF of
-- XS from the assumed CDF @cdf@.
--
-- If the data were, in fact, drawn from a distribution with the given CDF,
-- then the resulting p-value should be uniformly distributed over (0,1].
ksTest f xs = 1 - kCdf n d
    where
        n = length xs
        
        (kPlus, kMinus, d) = ks f n xs

-- |'KS' distribution: not really a standard mathematical concept, but still
-- a nice conceptual shift.  @KS n d@ is the distribution of a random
-- variable constructed as a list of @n@ random variables of distribution @d@.
-- 
-- The corresponding 'CDF' instance implements the K-S test for such lists.
-- For example, if @xs@ is a list of length 100 believed to contain Beta(2,5)
-- variates, then @cdf (KS 100 (Beta 2 5))@ is the K-S test for that distribution.
-- (Note that if @length xs@ is not 100, then the result will be 0 because
-- such lists cannot arise from that 'KS' distribution.  Somewhat arbitrarily,
-- all lists of \"impossible\" length are grouped at the bottom of the ordering
-- encoded by the 'CDF' instance.)
-- 
-- The 'KS' test can easily be applied recursively.
-- For example, if @d@ is a 'Distribution' of interest and you have 100 trials
-- each with 100 data points, you can test it by calling @cdf (KS 100 (KS 100 d))@.
data KS d a where
     KS :: !Int -> !(d a) -> KS d [a]

instance Eq (d a) => Eq (KS d [a]) where
    KS n1 d1 == KS n2 d2    = n1 == n2 && d1 == d2
instance Show (d a) => Show (KS d [a]) where
    showsPrec p (KS n d) = showParen (p > 10) 
        ( showString "KS " 
        . showsPrec 11 n
        . showChar ' '
        . showsPrec 11 d
        )

instance Distribution d a => Distribution (KS d) [a] where
    rvar (KS n d) = replicateM n (rvar d)

instance CDF d a => CDF (KS d) [a] where
    cdf (KS n dist) xs 
        | length (take (n+1) xs) == n       = 1 - kCdf n d
        | otherwise                         = 0
        where
            (kPlus, kMinus, d) = ks (cdf dist) n xs
        