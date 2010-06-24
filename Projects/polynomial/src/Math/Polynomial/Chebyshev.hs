{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Chebyshev where

import Math.Polynomial

-- |The Chebyshev polynomials of the first kind with 'Integer' coefficients.
ts :: [Poly Integer]
ts = poly LE [1] : 
    [ addPoly (poly LE [0, 1]    `multPoly` t_n)
              (poly LE [-1,0,1] `multPoly` u_n)
    | t_n <- ts
    | u_n <- us
    ]

-- The Chebyshev polynomials of the second kind with 'Integer' coefficients.
us :: [Poly Integer]
us = poly LE [0] : 
    [ addPoly t_n (multPoly u_n (poly LE [0,1]))
    | t_n <- ts
    | u_n <- us
    ]

-- |Compute the coefficients of the n'th Chebyshev polynomial of the first kind.
t :: Num a => Int -> Poly a
t n = poly LE . map fromInteger . polyCoeffs LE $ ts !! n

-- |Compute the coefficients of the n'th Chebyshev polynomial of the second kind.
u :: Num a => Int -> Poly a
u n = poly LE . map fromInteger . polyCoeffs LE $ us !! n

-- |Evaluate the n'th Chebyshev polynomial of the first kind at a point X.  
-- Both more efficient and more numerically stable than computing the 
-- coefficients and evaluating the polynomial.
evalT :: Num a => Int -> a -> a
evalT n x = evalTs x !! n

-- |Evaluate all the Chebyshev polynomials of the first kind at a point X.
evalTs :: Num a => a -> [a]
evalTs = fst . evalTsUs

-- |Evaluate the n'th Chebyshev polynomial of the second kind at a point X.  
-- Both more efficient and more numerically stable than computing the 
-- coefficients and evaluating the polynomial.
evalU :: Num a => Int -> a -> a
evalU n x = evalUs x !! n

-- |Evaluate all the Chebyshev polynomials of the second kind at a point X.
evalUs :: Num a => a -> [a]
evalUs = snd . evalTsUs

-- |Evaluate the n'th Chebyshev polynomials of both kinds at a point X.
evalTU :: Num a => Int -> a -> (a,a)
evalTU n x = (ts!!n, us!!n)
    where (ts,us) = evalTsUs x

-- |Evaluate all the Chebyshev polynomials of the both kinds at a point X.
evalTsUs :: Num a => a -> ([a], [a])
evalTsUs x = (ts, us)
    where
        ts = 1 : [x * t_n - (1-x*x)*u_n  | t_n <- ts | u_n <- us]
        us = 0 : [x * u_n + t_n          | t_n <- ts | u_n <- us]

-- |Compute the roots of the n'th Chebyshev polynomial of the first kind.
tRoots :: Floating a => Int -> [a]
tRoots   n = [cos (pi / fromIntegral n * (fromIntegral k + 0.5)) | k <- [0..n-1]]

-- |Compute the extreme points of the n'th Chebyshev polynomial of the first kind.
tExtrema :: Floating a => Int -> [a]
tExtrema n = [cos (pi / fromIntegral n *  fromIntegral k       ) | k <- [0..n]]

-- |Estimate the first N coefficients of the Chebyshev series expansion of a
-- given function.
chebyshevFit :: Floating a => Int -> (a -> a) -> [a]
chebyshevFit n f = 
    [ 2 / fromIntegral n * sum 
        [ f x * evalT j x
        | x <- tRoots n
        ]
    | j <- [0..n-1]
    ]

-- |Evaluate a Chebyshev series expansion with a finite number of terms.
evalChebyshevSeries :: Fractional a => [a] -> a -> a
evalChebyshevSeries []      = \x -> 0
evalChebyshevSeries (c0:cs) = \x -> sum (zipWith (*) (0.5 * c0 : cs) (evalTs x))
