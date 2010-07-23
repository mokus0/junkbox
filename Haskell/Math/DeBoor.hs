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
-- j'th segment of the spline, if and only if the parameter falls within
-- the corresponding segment of the knot vector.  The overall degree of
-- the spline is an input as well because it determines the shape
-- of each basis function.
--
-- Note that the algorithm does not make use of @us!!0@ at all.  I 
-- am not completely sure, but I think this is correct.  I believe this
-- is because De Boor's recurrence requires an irrelevant choice of 
-- extra control point that would be, notionally, @ds!!(-1)@.  This
-- control point has no actual influence inside the domain of the spline,
-- although it /can/ affect values outside the domain (The domain of a
-- B-spline is the central portion of the knot vector where there are 
-- enough basis functions \"active\" to form a complete basis).
-- 
-- This implementation makes the choice that that control point be identical
-- to the first, so that the position of the first knot is irrelevant.
-- This function could alternatively be written to take the extra control
-- point as an argument or as a part of @ds@, in which case slightly 
-- different logic would be required in the driver function 'bspline'.
--
-- I find this result difficult to convince myself of, though, even
-- though the explanation seems plausible.  The reason is that it seems
-- to indicate that the position of the first knot is utterly irrelevant.
-- Empirically, though, this seems to make sense, as the position of
-- the last knot also seems to be irrelevant (at least, assuming my 
-- implementation is sound)
deBoor' p us ds x = go us (padTo uHi zeroV ds)
    where
        -- Upper endpoints of the intervals are the same for
        -- each row in the table:
        uHi = drop (p+1) us
        
        -- On each pass, the lower endpoints of the 
        -- interpolation intervals advance and the new 
        -- coefficients are given by linear interpolation
        -- on the current intervals, just as in 
        -- De Casteljau's algorithm (this is the main
        -- generalization: in a Bezier spline,
        -- the interval is always [0,1], because
        -- the knot vector is always 
        -- @replicate (p+1) 0 ++ replicate (p+1) 1@).
        go       _ [] = []
        go (_:uLo) ds = ds : go uLo ds'
            where
                ds' = zipWith4 (interp x) uLo uHi
                                          ds (tail ds)

-- Simpler, clearer, slightly less efficient and less tolerant of
-- improper inputs:
deBoor p      _ [] x = []
deBoor p (_:us) ds x = ds : deBoor (p-1) us ds' x
    where
        ds' = zipWith (interp x) (spans p us) (spans 1 ds)

        spans n xs = zip xs (drop n xs)
        
        interp x (x0,x1) (y0,y1)
            |  x <  x0  = y0
            |  x >= x1  = y1
            | otherwise = lerp y0 y1 a
            where
                a = (x - x0) / (x1 - x0)


-- Note: returns results on a larger domain than the domain of the spline.
-- This is to avoid problems with minor errors in the input parameter, especially
-- in cases where the first or last knot is not an IEEE-exact number.  It is
-- up to the user to check the domain on which they evaluate the spline.
-- Outside the normal bounds, the basis functions gradually lose polynomial
-- degree (although this behavior is subject to change).  The first and last
-- basis functions go to 1 outside the knot vector.  Thus, outside the knot
-- vector the spline takes the value of the control point at the 
-- corresponding end of the spline.
bspline p us ds
    | lastSegment < 0   = error "bspline: not enough control points"
    | otherwise         = \x -> deBoor p us ds x !! p !! segment x
    where
        lastSegment = length us - 2*(p+1)
        segment x   = clip 0 lastSegment (count (<x) (drop p us) - 1)

-- a few very general utility functions
count p = length . filter p
clip lo hi = max lo . min hi
trimTo list  xs = zipWith const xs list
padTo list z xs = trimTo list (xs ++ repeat z)

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
        ds = [ ((-1, 0),1)
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

-- basis splines for a given knot vector
basis :: Int -> [Double] -> Int -> Double -> Double
basis p us i = bspline p us ds
    where
        m  = length us - p - 1
        ds = take m (replicate i 0 ++ 1 : repeat 0)