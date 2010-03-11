{-# LANGUAGE RecordWildCards #-}
module NR.Ch5.S9 where

import NR.Ch5.S8

import Data.Vector.Generic (Vector, generate, (!))
import qualified Data.Vector
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.List

import Control.Monad.ST
import Data.STRef

import Control.Arrow

chebyshevDerivative
  :: (Real a, Fractional b, Vector v1 b, Vector v2 b) =>
     Chebyshev v1 a b -> Chebyshev v2 a b
chebyshevDerivative Chebyshev{..} = Chebyshev
    { chebOrder     = chebOrder - 1
    , chebCoeffs    = normalized
    , ..}
    
    where
        n = V.length chebCoeffs - 1
        cder = generate n $ \j -> let i = j + 1
                                   in cderOrZero (i+1) + 2 * fromIntegral i * (chebCoeffs!i)
        
        cderOrZero i    | i >= n    = 0
                        | otherwise = cder!i
                        
        con = 2 / (realToFrac chebB - realToFrac chebA)
        normalized = (generate n $ \j -> con * (cder!j)) `asTypeOf` cder

chebyshevIntegral :: (Real a, Fractional b, Vector v1 b, Vector v2 b)
    => Chebyshev v1 a b -> Chebyshev v2 a b
chebyshevIntegral Chebyshev{..} = Chebyshev
    { chebCoeffs    = cint
    , ..}
        
    where
        n = V.length chebCoeffs
        con = 0.25 * realToFrac (chebB - chebA)
        cint = runST $ do
            cint <- MV.newWith n 0
            fac  <- newSTRef 1
            csum <- newSTRef 0
            
            sequence_
                [ do
                    let cint_j = con * ((chebCoeffs!(j-1)) - (chebCoeffs!(j+1))) / fromIntegral j
                    MV.write cint j cint_j
                    
                    f <- readSTRef fac
                    writeSTRef fac (negate f)
                    
                    modifySTRef csum (+ (f * cint_j))
                | j <- [1 .. n-2]
                ]
            
            let cint_nm1 = con * (chebCoeffs!(n-2)) / (fromIntegral n - 1)
            MV.write cint (n-1) cint_nm1
            
            f <- readSTRef fac
            modifySTRef csum (+ (f * cint_nm1))
            
            csum <- readSTRef csum
            MV.write cint 0 (2 * csum)
            
            V.unsafeFreeze cint


-- |Quadrature routine based on fitting a chebyshev series and integrating
clenshawCurtis f order a b = chebyshevIntegralAB (cheb f order a b `asTypeOf` (undefined :: Chebyshev Data.Vector.Vector a b))

-- |Fairly naive and inefficient adaptive version of 'clenshawCurtis'. 
-- The result is a list of rejected steps and (if convergent) the accepted one.
clenshawCurtisAdaptive f a b eps = second head . break ((<eps).abs.snd) $
    [ clenshawCurtis f order a b eps
    | order <- iterate (*2) 16
    ]

-- quick integral over whole fitted range
chebyshevIntegralAB Chebyshev{..} eps = (con * sum termsUsed, err)
    where
        con = realToFrac (chebB - chebA)
        (termsUsed, err) = case span (\term -> abs term >= eps) terms of
            (ok, e:_)   -> (ok, e)
            (ok, [])    -> (init ok, last ok)
        terms = 0.5 * (chebCoeffs!0) :
            [ negate (chebCoeffs!k2) / fromIntegral ((k2+1) * (k2-1))
            | k2    <- [2, 4 .. V.length chebCoeffs - 1]
            ]
