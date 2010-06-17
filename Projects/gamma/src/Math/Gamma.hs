module Math.Gamma
    ( Gamma(..)
    , Factorial(..)
    , IncGamma(..)
    ) where

import Math.Gamma.Stirling (lnGammaStirling)
import Math.Gamma.Lanczos (reflect, gammaLanczos, lnGammaLanczos)
import qualified Data.Vector.Unboxed as V

import qualified NR.Ch6.S2 as NR

-- |Gamma function.  Minimal definition is ether gamma or lnGamma.
class Floating a => Gamma a where
    -- |The gamma function:  gamma z == integral from 0 to infinity of
    -- @\t -> t**(z-1) * exp (negate t)@
    gamma :: a -> a
    gamma 0 = 0/0
    gamma z
        | z == abs z    = exp (lnGamma z)
        | otherwise     = pi / (sin (pi * z) * exp (lnGamma (1-z)))


    -- |Natural log of the gamma function
    lnGamma :: a -> a
    lnGamma z = log (gamma z)
    
    -- |Natural log of the factorial function
    lnFactorial :: Integral b => b -> a
    lnFactorial n = lnGamma (fromIntegral n+1)

instance Gamma Float where
    gamma = realToFrac . reflect (gammaLanczos g cs) . realToFrac
        where
            g :: Double
            g = pi
            cs = [1.0000000249904433,9.100643759042066,-4.3325519094475,
                  0.12502459858901147,1.1378929685052916e-4,-9.555011214455924e-5]

    
    lnGamma z
        | z <= 0    = error "lnGamma: z <= 0"
        | otherwise = realToFrac (gam (realToFrac z))
        where
            -- Stirling's approximation takes very large numbers of terms to converge when z is small,
            -- so repeatedly shift it up until 32 terms will suffice.
            gam z
                | z < 6     = gam (z+1) - log z
                | otherwise = lnGammaStirling cs z
            cs = [8.333333333333333e-2,8.333333333333333e-2,0.16388888888888886,0.4833333333333333,1.903571428571429,9.386904761904761,55.627182539682536,385.06111111111113,3049.370286195286,27190.662878787887,269592.44589993346,2942145.3460622714,3.50467952851801e7,4.524846280013889e8,6.294024232904708e9,9.38382340807317e10,1.492815245447996e12,2.524017944507227e13,4.519816875674298e14,8.54550564657986e15]

    
    lnFactorial n
        | n' < 0                = error "lnFactorial n: n < 0"
        | n' < toInteger nFacs  = facs V.! fromIntegral n
        | otherwise             = lnGamma (fromIntegral n+1)
        where
            n' = toInteger n
            nFacs       = 2000 -- limited only by time and space
            facs        = V.map lnGamma (V.enumFromN 1 nFacs)

instance Gamma Double where
    gamma = reflect (gammaLanczos g cs)
        where
            g = 2*pi
            cs = [1.0000000000000002,311.60117505414695,-498.65119046033163,244.08472899875767,-38.67036462939322,1.3350899103585203,-1.8972831806242229e-3,-3.935368195357295e-7,2.592464641764731e-6,-3.2263565156368265e-6,2.5666169886566876e-6,-1.3737776806198937e-6,4.4551204024819644e-7,-6.576826592057796e-8]

    lnGamma z
        | z <= 0    = error "lnGamma: z <= 0"
        | otherwise = lnGammaLanczos g cs z
        where
            g = exp pi / pi
            cs = [1.0000000000000002,1002.5049417114732,-1999.6140446432912,1352.1626218340114,-360.6486475548049,33.344988357090685,-0.6637188712004668,5.16644552377916e-4,1.684651140163429e-7,-1.8148207145896904e-7,6.171532716135051e-8,-9.014004881476154e-9]

    lnFactorial n
        | n' < 0                = error "lnFactorial n: n < 0"
        | n' < toInteger nFacs  = facs V.! fromIntegral n
        | otherwise             = lnGamma (fromIntegral n+1)
        where
            n' = toInteger n
            nFacs       = 2000 -- limited only by time and space
            facs        = V.map lnGamma (V.enumFromN 1 nFacs)

-- |Incomplete gamma functions.  Minimal definition is either 'p' or 'q', preferably both.
class Gamma a => IncGamma a where
    -- |Lower gamma function: lowerGamma s x == integral from 0 to x of 
    -- @\t -> t**(s-1) * exp (negate t)@
    lowerGamma :: a -> a -> a
    lowerGamma s x = exp (lnLowerGamma s x)
    -- |Upper gamma function: lowerGamma s x == integral from x to infinity of 
    -- @\t -> t**(s-1) * exp (negate t)@
    upperGamma :: a -> a -> a
    upperGamma s x = exp (lnUpperGamma s x)
    
    -- |Natural log of lower gamma function
    lnLowerGamma :: a -> a -> a 
    lnLowerGamma s x = lnGamma s + log (p s x)
    -- |Natural log of upper gamma function
    lnUpperGamma :: a -> a -> a
    lnUpperGamma s x = lnGamma s + log (q s x)
    
    -- |Regularized lower incomplete gamma function: lowerGamma z / gamma z
    p :: a -> a -> a
    p s x = 1 - q s x
    -- |Regularized upper incomplete gamma function: upperGamma z / gamma z
    q :: a -> a -> a
    q s x = 1 - p s x

instance IncGamma Float where
    p s x = realToFrac $ (NR.gammp :: Double -> Double -> Double) (realToFrac s) (realToFrac x)
    q s x = realToFrac $ (NR.gammq :: Double -> Double -> Double) (realToFrac s) (realToFrac x)
instance IncGamma Double where
    p = NR.gammp
    q = NR.gammq

-- |Factorial function
class Num a => Factorial a where
    factorial :: Integral b => b -> a
    factorial = fromInteger . factorial

instance Factorial Integer where
    factorial n = product [1..toInteger n]

instance Factorial Float where
    factorial = realToFrac . (factorial :: Integral a => a -> Double)
instance Factorial Double where
    factorial n
        | n < 0        = error "factorial: n < 0"
        | n < nFacs    = facs V.! fromIntegral n
        | otherwise     = infinity
        where
            nFacs :: Num a => a
            nFacs       = 171 -- any more is pointless, everything beyond here is "Infinity"
            facs        = V.scanl (*) 1 (V.enumFromN 1 nFacs)
            infinity    = facs V.! nFacs



binomialCoefficient :: (Integral a, Integral b) => a -> a -> b
binomialCoefficient n k
    | n < 0     = error "binomialCoefficient n k: n < 0"
    | k < 0     = error "binomialCoefficient n k: k < 0"
    | k > n     = error "binomialCoefficient n k: k > n"
    | n < 171   = floor (0.5 + factorial n' / (factorial k' * factorial (n'-k')) :: Double)
    | otherwise = floor (0.5 + exp (lnBinomialCoefficient n k)                   :: Double)
    where
        n' = fromIntegral n; k' = fromIntegral k

lnBinomialCoefficient :: (Integral a, Gamma b) => a -> a -> b
lnBinomialCoefficient n k
    | n < 0     = error "lnBinomialCoefficient n k: n < 0"
    | k < 0     = error "lnBinomialCoefficient n k: k < 0"
    | k > n     = error "lnBinomialCoefficient n k: k > n"
    | otherwise = lnFactorial n - lnFactorial k - lnFactorial (n-k)

beta :: Gamma a => a -> a -> a
beta z w = exp (lnGamma z + lnGamma w - lnGamma (z+w))
