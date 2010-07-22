{-# LANGUAGE TypeFamilies #-}
module Math.DeBoor where

import Data.List
import Data.VectorSpace

-- Simple linear interpolation on the interval [x0,x1], with output
-- clamped to the interval [y0, y1]
interp x x0 x1 y0 y1
    |  x <  x0  = y0
    |  x >= x1  = y1
    | otherwise = lerp y0 y1 a
    where
        a = (x - x0) / (x1 - x0)

-- Compute the table from deBoor's algorithm.  The i'th row contains
-- the i'th order polynomial terms.  The j'th term in each row is the
-- j'th segment in the spline of that order.  The overall degree of
-- the spline is an input as well because it determines the shape
-- of each basis function.
deBoor n x us ds = go us ds
    where
        -- Upper endpoints of the intervals are the same for
        -- each row in the table:
        uHi = drop n us
        
        -- On each pass, the lower endpoints of the 
        -- interpolation intervals advance and the new 
        -- coefficients are given by linear interpolation
        -- on the current intervals, just as in 
        -- De Casteljau's algorithm (this is the main
        -- generalization: in a Bezier spline,
        -- the interval is always [0,1]).
        go   _ [] = []
        go uLo ds = ds : go (drop 1 uLo) ds'
            where
                ds' = zipWith4 (interp x) uLo uHi
                                          ds (tail ds)
        
-- -- Slightly more readable form, slightly less efficient
-- -- and subtly "wrong" when evaluating past row n, which 
-- -- shouldn't be done anyway.
-- deBoor _ _  _ [] = []
-- deBoor _ _ [] ds = [ds]
-- deBoor n x us ds = ds : deBoor (n-1) x (tail us) ds'
--     where
--         ds' = zipWith4 (interp x) us (drop n us)
--                                   ds (tail   ds)

-- Note: defines the spline on a larger domain than is traditional - extends past
-- the ends by extrapolating the end segments.  This is to avoid problems with
-- minor errors in the input parameter, especially in cases where the first
-- or last knot is not an IEEE-exact number.  It is up to the user to decide 
-- precisely what domain is appropriate and only evaluate inside that domain.
bspline n us ds
    | lastSegment < 0   = error "bspline: not enough control points"
    | otherwise         = \x -> deBoor n x us ds !! n !! segment x
    where
        lastSegment = min (length us) (length ds) - n - 1
        segment x   = clip 0 lastSegment (count (<x) us - n)

-- a couple very general utility functions
count p = length . filter p
clip lo hi = max lo . min hi

nurbs
  :: (VectorSpace v, Scalar v ~ s,
      VectorSpace s, Scalar s ~ Scalar v,
      Fractional s, Ord s) =>
     Int -> [s] -> [(v, s)] -> s -> v
nurbs n us dws = project . bspline n us (map homogenize dws)
    where
        project (p,w) = recip w *^ p
        homogenize (d,w) = (w *^ d, w)

-- Example: a NURBS circle (0 <= x <= 1)
circle :: Double -> (Double, Double)
circle = nurbs 2 us ds
    where
        us = [0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1]
        ds = [ ((-1,-1),w)
             , ((-1, 0),1)
             , ((-1, 1),w)
             , (( 0, 1),1)
             , (( 1, 1),w)
             , (( 1, 0),1)
             , (( 1,-1),w)
             , (( 0,-1),1)
             , ((-1,-1),w)
             , ((-1, 0),1)
             ]
        w = sqrt 0.5
