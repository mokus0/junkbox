module Math.DeBoor where

import Data.List
import Data.VectorSpace

interp t lo hi x y = lerp x y a
    where
        a = (t - lo) / (hi - lo)

deBoor _ _  _ [] = []
deBoor _ _ [] ds = [ds]
deBoor n x us ds = ds : deBoor (n-1) x (tail us) ds'
    where
        ds' = zipWith4 (interp x) us (drop n us)
                                  ds (tail   ds)

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
