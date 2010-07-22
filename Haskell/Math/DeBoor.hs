module Math.DeBoor where

import Data.List
import Data.VectorSpace

interp t lo hi x y
    | hi == lo  = if t < hi then x else y
    | otherwise = lerp x y a
    where
        a = (t - lo) / (hi - lo)

deBoor n x us ds = go us ds
    where
        uHi = drop n us
        
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
--
-- Note 2: I'm not 100% sure the bounds checking is right.
bspline n us ds
    | lastSegment < 0   = error "bspline: not enough control points"
    | otherwise         = \x -> deBoor n x us ds !! n !! segment x
    where
        lastSegment = min (length us) (length ds) - n - 2
        segment x   = clip 0 lastSegment (count (<x) us - n)

-- a couple very general utility functions
count p = length . filter p
clip lo hi = max lo . min hi

nurbs
  :: (VectorSpace v, Scalar v ~ s,
      VectorSpace s, Scalar s ~ s, Fractional s, Ord s) =>
     Int -> [s] -> [(v, s)] -> s -> v
nurbs n us dws = project . bspline n us (map homogenize dws)
    where
        project (p,w) = recip w *^ p
        homogenize (d,w) = (w *^ d, w)

-- Example: a NURBS circle (0 <= x <= 1)
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
             ] :: [((Double, Double), Double)]
        w = sqrt 0.5
