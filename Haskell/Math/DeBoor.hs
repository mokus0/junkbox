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
        u_js = drop n us
        ds' = zipWith4 (interp x) us u_js ds (tail ds)

bspline n x us ds 
    | l   < 0   = outside
    | l+1 < n   = error "bspline: not enough knots below x"
    | l+n > length us
                = error "bspline: not enough knots above x"
    | l >= length ds
                = error "bspline: not enough control points"
    | otherwise = deBoor n x us ds !! n !! (l-n+1)
    where
        outside = error "bspline: x outside knot vector"
        
        l = case findIndex (> x) us of
            Nothing -> case findIndex (== x) us of
                Nothing -> outside
                Just i -> i-1
            Just i  -> i-1
