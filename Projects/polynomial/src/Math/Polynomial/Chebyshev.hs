{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Chebyshev where

import Math.Polynomial

-- |The Chebyshev polynomials of the first kind with 'Rational' coefficients.
ts :: [Poly Rational]
ts = poly LE [1] : 
    [ addPoly (poly LE [0, 1]    `multPoly` t_n)
              (poly LE [-1,0,1] `multPoly` u_n)
    | t_n <- ts
    | u_n <- us
    ]

-- The Chebyshev polynomials of the second kind with 'Rational' coefficients.
us :: [Poly Rational]
us = poly LE [0] : 
    [ addPoly t_n (multPoly u_n (poly LE [0,1]))
    | t_n <- ts
    | u_n <- us
    ]

-- |Compute the coefficients of the n'th Chebyshev polynomial of the first kind.
t :: Fractional a => Int -> Poly a
t n = poly LE . map fromRational . polyCoeffs LE $ ts !! n

-- |Compute the coefficients of the n'th Chebyshev polynomial of the second kind.
u :: Fractional a => Int -> Poly a
u n = poly LE . map fromRational . polyCoeffs LE $ us !! n

-- |Evaluate the n'th Chebyshev polynomial of the first kind at a point X.  
-- Both more efficient and more numerically stable than computing the 
-- coefficients and evaluating the polynomial.
evalT :: Fractional a => Int -> a -> a
evalT n x = evalTs x !! n

-- |Evaluate all the Chebyshev polynomials of the first kind at a point X.
evalTs :: Fractional a => a -> [a]
evalTs x = ts
    where
       ts = 1 : x : 
            [ 2 * x * t_n - t_nm1
            | n     <- iterate (1+) 1
            | t_n   <- tail ts
            | t_nm1 <- ts
            ]

-- |Evaluate the n'th Chebyshev polynomial of the second kind at a point X.  
-- Both more efficient and more numerically stable than computing the 
-- coefficients and evaluating the polynomial.
evalU :: Fractional a => Int -> a -> a
evalU n x = evalUs x !! n

-- |Evaluate all the Chebyshev polynomials of the second kind at a point X.
evalUs :: Fractional a => a -> [a]
evalUs x = us
    where
       us = 0 : 1 : 
            [ 2*x*u_n - u_nm1
            | n     <- iterate (1+) 1
            | u_n   <- tail us
            | u_nm1 <- us
            ]

tRoots :: Floating a => Int -> [a]
tRoots   n = [cos (pi / fromIntegral n * (fromIntegral k + 0.5)) | k <- [0..n-1]]

tExtrema :: Floating a => Int -> [a]
tExtrema n = [cos (pi / fromIntegral n *  fromIntegral k       ) | k <- [0..n]]