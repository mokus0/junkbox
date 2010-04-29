{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ViewPatterns #-}
module NR.Ch9.S3 where

import NR.Ch9.S1

data Brent a b = Brent
    { brA   :: !a
    , brFA  :: !b
    , brB   :: !a
    , brFB  :: !b
    , brC   :: !a
    , brFC  :: !b
    , brD   :: a
    , brE   :: a
    } deriving (Eq, Show)

class Eps a where
    -- |Smallest number for which 1 + eps > 1
    eps :: a

instance Eps Double where eps = encodeFloat 1 (-52)
instance Eps Float  where eps = encodeFloat 1 (-23)

-- TODO: clean up this mess!
instance (Eps a, Fractional a, Ord a) => RootFinder Brent a a where
    initRootFinder f x1 x2 = reorder (Brent x1 f1 x2 f2 x2 f2 dx dx)
        where f1 = f x1; f2 = f x2; dx = x1 - x2
    
    stepRootFinder f r@(Brent a fa b fb c fc d e)
        | abs e >= tol1 && abs fa > abs fb = iquad f xm tol1 r
        | otherwise = bisect f xm tol1 r
        where
            xm = 0.5 * (c - b)
            tol1 = 2 * eps * abs b + 0.5 * tol
            tol = 0

    estimateRoot Brent{brB = b} = b
    estimateError Brent{brB = b, brC = c} = 0.5 * (c - b)
    converged tol br@Brent{brB = b} = abs (estimateError br) <= tol1
        where
            tol1 = 2 * eps * abs b + 0.5 * tol

reorder orig@(Brent a fa b fb c fc d e)
    |  (fb > 0 && fc > 0)
    || (fb < 0 && fc < 0)
    =   let d' = b-a
         in reorder (Brent a fa b fb a fa d' d')
    | abs fc < abs fb
    = Brent  b fb a fa b fb d e
    | otherwise = orig

iquad f xm tol1 orig@(Brent a fa b fb c fc d e)
    | 2 * p < min min1 min2 = finish f xm tol1 (Brent a fa b fb c fc (p/q) d)
    | otherwise             = bisect f xm tol1 orig
    
    where
        s = fb / fa
        p = abs p'
        q = if p' > 0 then q' else negate q'
        (p',q')   | a == c    = (2 * xm * s, 1 - s)
                | otherwise = let t = fa / fc
                                  r = fb / fc
                               in ( s * (2 * xm * t * (t - r) - (b - a) * (r - 1))
                                  , (t - 1) * (r - 1) * (s - 1)
                                  )
        min1 = 3 * xm * q - abs (tol1 * q)
        min2 = abs (e * q)

bisect f xm tol1 (Brent a fa b fb c fc d e) = finish f xm tol1 (Brent a fa b fb c fc xm xm)

finish f xm tol1 (Brent a fa b fb c fc d e) = reorder (Brent b fb b' fb' c fc d e)
    where
        b' = if abs d > tol1 then b + d else b + abs tol1 * signum xm
        fb' = f  b'

zbrent f x1 x2 xacc = 
    (estimateRoot :: (Fractional a, Ord a, Eps a) => Brent a a -> a)
    (findRoot f x1 x2 xacc)
