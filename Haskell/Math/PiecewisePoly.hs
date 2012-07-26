{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Math.Spline.PiecewisePoly where

import Data.VectorSpace
import Math.Polynomial
import Math.Spline.Class
import Math.Spline.Knots

data PiecewisePoly a = PiecewisePoly (Scalar a) [(Scalar a, Poly a)]

instance (VectorSpace a, Ord (Scalar a), Fractional (Scalar a)) => Spline PiecewisePoly a where
    splineDomain (PiecewisePoly _ []) = Nothing
    splineDomain (PiecewisePoly x0 segments) = Just (x0, x0 + sum (map fst segments))
    evalSpline (PiecewisePoly x0 segments) x = go segments (x - x0)
        where
            go []           x   = zeroV
            go ((w,f):rest) x
                | x < w         = evalPoly' f (max 0 x)
            go [(w,f)]      x   = evalPoly' f (min w x)
            go ((w,f):rest) x   = go rest (x-w)
    splineDegree (PiecewisePoly _ segments) = maximum (map (polyDegree' . snd) segments)
    knotVector (PiecewisePoly x0 segments) = mkKnots (scanl (+) x0 (map fst segments))
    
    -- TODO: work out toBSpline

evalPoly' :: VectorSpace a => Poly a -> Scalar a -> a
evalPoly' = undefined

polyDegree' :: VectorSpace a => Poly a -> Int
polyDegree' = undefined