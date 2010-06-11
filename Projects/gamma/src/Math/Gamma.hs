module Math.Gamma where

import Math.Gamma.Stirling (lnGammaStirling)
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
    lnGamma z
        | z <= 0    = error "lnGamma: z <= 0"
        | otherwise = gam z
        where
            -- Stirling's approximation takes very large numbers of terms to converge when z is small,
            -- so repeatedly shift it up until about 40 terms will suffice.
            gam z
                | z < 15    = gam (z+1) - log z
                | otherwise = lnGammaStirling cs z
            cs = [8.333333333333333e-2,8.333333333333333e-2,0.16388888888888886,0.4833333333333333,1.903571428571429,9.386904761904761,55.627182539682536,385.06111111111113,3049.370286195286,27190.662878787887,269592.44589993346,2942145.3460622714,3.50467952851801e7,4.524846280013889e8,6.294024232904708e9,9.38382340807317e10,1.492815245447996e12,2.524017944507227e13,4.519816875674298e14,8.54550564657986e15,1.701096502429735e17,3.5563045547165394e18,7.790305587568763e19,1.7843945212560584e21,4.265604690290731e22,1.0623408958587036e24,2.751951339532148e25,7.403960785750008e26,2.066018059231738e28,5.971632845044214e29,1.7857526486045162e31,5.5186535788731415e32,1.7606535181259371e34,5.793179130771491e35,1.9640891488262395e37,6.855336982941969e38,2.4612930067367653e40,9.082968882150484e41,3.442733780636743e43,1.3393308411812385e45]


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
