{-# LANGUAGE ParallelListComp #-}
module Math.Legendre where

import NR.Ch5.S1

-- |List of the legendre polynomials with 'Rational' coefficients.
legendres :: [Poly Rational]
legendres = ps
    where
        x = polyLE [0,1]
        poly /. n = poly * polyLE [recip n]
        ps = 1 : x : 
            [ ((2 * fromInteger n + 1) * x * p_n - fromInteger n * p_nm1) /. (fromInteger n + 1)
            | n     <- [1..]
            | p_n   <- tail ps
            | p_nm1 <- ps
            ]

-- |Compute the coefficients of the n'th Legendre polynomial.
legendre :: Fractional a => Int -> Poly a
legendre n = polyLE . map fromRational . polyCoeffsLE $ legendres !! n

-- |Evaluate the n'th Legendre polynomial at a point X.  Both more efficient
-- and more numerically stable than computing the coefficients and evaluating
-- the polynomial.
evalLegendre n x = evalLegendres x !! n

-- |Evaluate all the Legendre polynomials at a point X.
evalLegendres x = ps
    where
       ps = 1 : x : 
            [ ((2 * n + 1) * x * p_n - n * p_nm1) / (n + 1)
            | n     <- iterate (1+) 1
            | p_n   <- tail ps
            | p_nm1 <- ps
            ]

-- |Evaluate the n'th Legendre polynomial and its derivative at a point X.  
-- Both more efficient and more numerically stable than computing the
-- coefficients and evaluating the polynomial.
evalLegendreDeriv 0 x = (1,0)
evalLegendreDeriv n x = case drop (n-1) (evalLegendres x) of
    (p2:p1:_)   -> (p1, fromIntegral n * (x * p1 - p2) / (x*x - 1))