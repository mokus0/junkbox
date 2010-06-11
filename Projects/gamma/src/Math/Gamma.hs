module Math.Gamma where

import qualified NR.Ch6.S1 as NR
import qualified NR.Ch6.S2 as NR

-- |Gamma function.  Minimal definition is ether gamma or lnGamma.
class Floating a => Gamma a where
    -- |The gamma function:  gamma z == integral from 0 to infinity of
    -- @\t -> t**(z-1) * exp (negate t)@
    gamma :: a -> a
    gamma z = exp (lnGamma z)
    
    -- |Natural log of the gamma function
    lnGamma :: a -> a
    lnGamma z = log (gamma z)
    
    -- |Natural log of the factorial function
    lnFactorial :: Integer -> a
    lnFactorial n = lnGamma (fromIntegral n+1)

instance Gamma Float where
    lnGamma = realToFrac . (NR.gammln :: Double -> Double) . realToFrac

instance Gamma Double where
    lnGamma = NR.gammln
    lnFactorial = NR.factln

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
    factorial :: Int -> a
    factorial = fromInteger . factorial

instance Factorial Integer where
    factorial n = product [1..toInteger n]

instance Factorial Float where
    factorial = realToFrac . (NR.factorial :: Int -> Double)
instance Factorial Double where
    factorial = NR.factorial