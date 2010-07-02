{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Lagrange
    ( lagrangeBasis
    , lagrange
    , lagrangeWeights
    ) where

import Math.Polynomial (Poly, poly, Endianness(..), multPoly)

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

lagrangeBasis :: Fractional a => [a] -> [Poly a]
lagrangeBasis xs =
    [ foldl1 multPoly
        [ poly LE [negate x_j/q, 1/q]
        | x_j <- otherXs
        , let q = x_i - x_j
        ]
    | (x_i, otherXs) <- select xs
    ]

lagrange :: Num a => [a] -> Poly a
lagrange xs = foldl1 multPoly
    [ poly LE [negate x_i, 1]
    | x_i <- xs
    ]

lagrangeWeights :: Fractional a => [a] -> [a]
lagrangeWeights xs = 
    [ recip $ product
        [ x_i - x_j
        | x_j <- otherXs
        ]
    | (x_i, otherXs) <- select xs
    ]
