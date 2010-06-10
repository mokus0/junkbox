{-# LANGUAGE ParallelListComp, BangPatterns #-}
module NR.Ch4.S2 where

import Control.Monad.ST
import Data.Bits
import Data.List

-- |Divide an interval [a,b] into segments of width dx, and return
-- the midpoint of each segment
midpoints a b dx = takeWhile (< max a b) (iterate (+dx) x0)
    where
        x0 = min a b + 0.5 * abs dx

-- |@trapzd f a b@ computes a sequence of improving estimates of the
-- integral of f from a to b, using the trapezoid rule.
trapzd f a b = scanl next first (iterate (flip shiftL 1) 1)
    where
        first = 0.5 * (b - a) * (f a + f b)
        next !prev !n = 0.5 * (prev + dx * foldl1' (+) ys)
            where
                dx = (b - a) / fromIntegral (n :: Int)
                ys = map f (midpoints a b dx)

-- |@simp f a b@ computes a sequence of improving estimates of the
-- integral of f from a to b, using Simpson's rule.
simp f a b = 
    [ (4/3) * s_2n - (1/3) * s_n 
    | s_n : s_2n : _ <- tails (trapzd f a b)
    ]

-- |@qtrap f a b eps@ attempts to estimate the integral of f from a to b
-- to within a relative tolerance eps, using the trapezoid rule.
qtrap f a b eps = qconv "qtrap" 5 20 eps (trapzd f a b)

-- |@qsimp f a b eps@ attempts to estimate the integral of f from a to b
-- to within a relative tolerance eps, using Simpson's rule.
qsimp f a b eps = qconv "qsimp" 5 20 eps (simp f a b)

-- |general-purpose warm-start convergence test: @qconv loc jstart jmax eps@
-- returns a function that takes a list, skips jstart elements, and searches
-- the list up to jmax elements (from the start of the list) for a pair of
-- successive elements within (relative) eps of each other.  If no such pair
-- is found, throws an error using 'loc' as a part of the error string.  If
-- loc is "", then "qconv" is used instead.
qconv ""  jstart jmax eps = qconv "qconv" jstart jmax eps
qconv loc jstart jmax eps = converge jstart . drop (jstart-1)
    where
        err = error (loc ++ ": too many steps")
        
        converge j (x1:xs@(x2:_)) 
            | j >= jmax                     = err
            | x1 == 0                       = 0
            | abs (x1-x2) <= eps * abs x1   = x2
            | otherwise                     = converge (j+1) xs
        converge _ _ = err