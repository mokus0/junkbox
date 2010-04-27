{-# LANGUAGE ParallelListComp #-}
module Math.Legendre where

import NR.Ch5.S1

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

legendre :: Fractional a => Int -> Poly a
legendre n = polyLE . map fromRational . polyCoeffsLE $ legendres !! n

evalLegendre n x = evalLegendres x !! n
evalLegendres x = ps
    where
       ps = 1 : x : 
            [ ((2 * n + 1) * x * p_n - n * p_nm1) / (n + 1)
            | n     <- iterate (1+) 1
            | p_n   <- tail ps
            | p_nm1 <- ps
            ]

evalLegendreDeriv 0 x = (1,0)
evalLegendreDeriv n x = case drop (n-1) (evalLegendres x) of
    (p2:p1:_)   -> (p1, fromIntegral n * (x * p1 - p2) / (x*x - 1))