{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Legendre where

import Math.Polynomial

-- |The Legendre polynomials with 'Rational' coefficients.
legendres :: [Poly Rational]
legendres = poly LE [1] : poly LE [0,1] : 
    [ multPoly
        (poly LE [recip (n' + 1)])
        (addPoly (poly LE [0, 2 * n' + 1] `multPoly` p_n)
                 (poly LE           [-n'] `multPoly` p_nm1)
        )
    | n     <- [1..], let n' = fromInteger n
    | p_n   <- tail legendres
    | p_nm1 <- legendres
    ]

-- |Compute the coefficients of the n'th Legendre polynomial.
legendre :: Fractional a => Int -> Poly a
legendre n = poly LE . map fromRational . polyCoeffs LE $ legendres !! n

-- |Evaluate the n'th Legendre polynomial at a point X.  Both more efficient
-- and more numerically stable than computing the coefficients and evaluating
-- the polynomial.
evalLegendre :: Fractional a => Int -> a -> a
evalLegendre n x = evalLegendres x !! n

-- |Evaluate all the Legendre polynomials at a point X.
evalLegendres :: Fractional a => a -> [a]
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
evalLegendreDeriv :: Fractional a => Int -> a -> (a,a)
evalLegendreDeriv 0 x = (1,0)
evalLegendreDeriv n x = case drop (n-1) (evalLegendres x) of
    (p2:p1:_)   -> (p1, fromIntegral n * (x * p1 - p2) / (x*x - 1))
