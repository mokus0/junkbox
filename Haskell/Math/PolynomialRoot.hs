module Math.PolynomialRoot where

import Data.Complex
import Functions.Select (select)
import Math.Polynomial
import Math.Sequence.Converge

durandKerner eps p = convergeBy test Just (durandKerner' p)
    where
        magSq (a :+ b) = a*a + b*b
        n = fromIntegral (polyDegree p)
        norm = sum . map magSq
        test (xs0:xs1:_)
            | norm (zipWith subtract xs0 xs1) <= (n * eps^2)
            = Just xs1
        test _ = Nothing

-- A very simple polynomial root finder, effective for low degree polynomials
-- when the roots are well-separated.
durandKerner' p = go (take n (iterate (* (0.4 :+ 0.9)) 1))
    where
        n = polyDegree p
        f = evalPoly (monicPoly p)
        go cs = cs : go cs'
            where cs' = do
                    (c, cs) <- select cs
                    return (c - f c / product [c - c' | c' <- cs])
