{-# LANGUAGE ParallelListComp #-}
module NR.Ch6.S1 where

import qualified Data.Vector.Unboxed as V

class Gamma a where
    gammln :: a -> a
    factln :: Int -> a

class Factorial a where
    factorial   :: Int -> a

instance Gamma Double where
    gammln x
        | x <= 0   = error "gammln: x <= 0"
        | otherwise = tmp + log(2.5066282746310005*ser/x)
        where
            tmp' = x + 5.2421875
            tmp = (x+0.5) * log tmp' - tmp'
            ser = 0.999999999999997092
                + sum   [ cof / y
                        | cof <- cofs 
                        | y <- iterate (+1) (x+1)
                        ]
            cofs = 
                [  57.1562356658629235
                , -59.5979603554754912
                ,  14.1360979747417471
                , - 0.491913816097620199
                ,   0.339946499848118887e-4
                ,   0.465236289270485756e-4
                , - 0.983744753048795646e-4
                ,   0.158088703224912494e-3
                , - 0.210264441724104883e-3
                ,   0.217439618115212643e-3
                , - 0.164318106536763890e-3
                ,   0.844182239838527433e-4
                , - 0.261908384015814087e-4
                ,   0.368991826595316234e-5
                ]
    factln n
        | n < 0     = error "factln: n < 0"
        | n < nFacs =   facs V.! n
        | otherwise = gammln (fromIntegral n+1)
        where
            nFacs       = 2000 -- limited only by time and space
            facs        = V.map gammln (V.enumFromN 1 nFacs)

instance Factorial Double where
    factorial n 
        | n < 0        = error "factorial: n < 0"
        | n < nFacs    = facs V.! n
        | otherwise     = infinity
        where
            nFacs       = 171 -- any more is pointless, everything beyond here is "Infinity"
            facs        = V.scanl (*) 1 (V.enumFromN 1 nFacs)
            infinity    = facs V.! nFacs

bico n k
    | n < 0 || k < 0 || k > n   = error "bico: bad args"
    | n < 171                   = floor (0.5 + factorial n / (factorial k * factorial (n-k)) :: Double)
    | otherwise                 = floor (0.5 + exp (factln n - factln k - factln (n-k))      :: Double)

bicoln n k
    | n < 0 || k < 0 || k > n   = error "bicoln: bad args"
    | otherwise                 = factln n - factln k - factln (n-k)

beta z w = exp (gammln z + gammln w - gammln (z+w))