module Math.Gamma
    ( Gamma(..)
    , Factorial(..)
    , IncGamma(..)
    ) where

import Math.Gamma.Stirling (lnGammaStirling)
import Math.Gamma.Lanczos (reflect, gammaLanczos, lnGammaLanczos)
import qualified Data.Vector.Unboxed as V
import Data.List (sortBy)
import Data.Ord (comparing)

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
    -- |Natural log of lower gamma function
    lnLowerGamma :: a -> a -> a 
    lnLowerGamma s x = lnGamma s + log (p s x)
    -- |Regularized lower incomplete gamma function: lowerGamma z / gamma z
    p :: a -> a -> a
    p s x = 1 - q s x
    
    -- |Upper gamma function: lowerGamma s x == integral from x to infinity of 
    -- @\t -> t**(s-1) * exp (negate t)@
    upperGamma :: a -> a -> a
    upperGamma s x = exp (lnUpperGamma s x)
    -- |Natural log of upper gamma function
    lnUpperGamma :: a -> a -> a
    lnUpperGamma s x = lnGamma s + log (q s x)
    -- |Regularized upper incomplete gamma function: upperGamma z / gamma z
    q :: a -> a -> a
    q s x = 1 - p s x

instance IncGamma Float where
    p s x = realToFrac $ (p :: Double -> Double -> Double) (realToFrac s) (realToFrac x)
    q s x = realToFrac $ (q :: Double -> Double -> Double) (realToFrac s) (realToFrac x)
instance IncGamma Double where
    lowerGamma 0 0 = 0/0
    lowerGamma s x = sign (exp (log (abs x) * s - x) / s * m_1_sp1 s x)
            where
                sign 
                    | x < 0 = case properFraction s of
                        (sI, 0) | s < 0     -> const (0/0)
                                | even sI   -> id 
                                | otherwise -> negate
                        _                   -> const (0/0)
                    | otherwise = id
    
    -- TODO: properly handle x<0
    -- lnLowerGamma s x = s * log x - log s - x + log (m_1_sp1 s x)    
    
    p 0 0 = 0/0
    p s x
        | x >= s+1  = 1 - q s x
        | x < 0 
        = case properFraction s of
            (sI, 0) | s > 0 -> 1 - exp (-x) * sum (scanl (*) 1 [x / fromIntegral k | k <- [1 .. sI-1]]) -- [x^k / factorial k | k <- [0..sI-1]]
            _               -> 0/0
    
        | s < 0
        = sin (pi*s) / (-pi)
        * exp (s * log x - x + lnGamma  (-s)) * m_1_sp1 s x
    
        | s == 0 || x == 0
        = 0
    
        | otherwise
        = exp (s * log x - x - lnGamma (s+1)) * m_1_sp1 s x

    -- -- upperGamma s x = exp (-x) * (gamma s * convergingSum (scanl (*) 1 [x / n | n <- [1..]]) - x**s * m_1_sp1 s x / s)
    -- -- upperGamma s x = exp (lnGamma s - x) * (convergingSum (scanl (*) 1 [x / n | n <- [1..]]) - convergingSum (scanl (*) ((x**s)/gamma(s+1)) [x / n | n <- [s+1 ..]]))
    -- -- upperGamma s x  = exp (lnGamma s - x) 
    -- --                 * convergingSum (zipWith (-) (scanl (*) 1 [x / n | n <- [1..]]) 
    -- --                                              (scanl (*) ((x**s)/gamma(s+1)) [x / n | n <- [s+1 ..]])
    -- --                                              )
    -- upperGamma s x  = exp (lnGamma s - x) 
    --                 * (sum series0 + convergingSum (zipWith (-) series1 series2))
    --     where
    --         (series0, series1) = splitAt (ceiling s) seriesA
    --         seriesA = scanl (*) 1 [x / n | n <- [1..]]
    --         series2 = scanl (*) ((x**s)/gamma(s+1)) [x / n | n <- [s+1 ..]]
    -- 
    -- -- lnUpperGamma s x = lnGamma s - x + log (convergingSum (scanl (*) 1 [x / n | n <- [1..]]) - convergingSum (scanl (*) ((x**s)/gamma(s+1)) [x / n | n <- [s+1 ..]]))
    -- -- lnUpperGamma s x  = lnGamma s - x
    -- --                   + log (convergingSum (zipWith (-) (scanl (*) 1 [x / n | n <- [1..]]) 
    -- --                                                     (scanl (*) ((x**s)/gamma(s+1)) [x / n | n <- [s+1 ..]])
    -- --                                                     ))
    -- lnUpperGamma s x  = lnGamma s - x
    --                 + log (sum0 + convergingSumRat (1e-16 * abs sum0) 1e-16 (zipWith (-) series1 series2))
    --     where
    --         sum0 = sum series0
    --         (series0, series1) = splitAt (ceiling s) seriesA
    --         seriesA = scanl (*) 1 [x / n | n <- [1..]]
    --         series2 = scanl (*) ((x**s)/gamma(s+1)) [x / n | n <- [s+1 ..]]

    q 0 x = 1
    q s x  
        | x <= 0 || x < s+1 = 1 - p s x
        | otherwise
        = case properFraction s of
            (sI, 0) | s >= 0 
                    -> fromRational $ sum (take sI series1)
            _       -> convergingSumRat 0 1e-16 (sum series1a : zipWith (-) series1b series2)
            where
                lnX = log x
                (series1a, series1b) = splitAt (max 0 (floor s)) series1
                -- series1 = map exp $ scanl (+) (-x)                       [lnX - log n | n <- [1..]]
                -- series2 = map exp $ scanl (+) (lnX*s - lnGamma(s+1) - x) [lnX - log n | n <- [s+1 ..]]
                series1 = map (toRational (exp (-x)) *) $ scanl (*) (1)                     [toRational x/n | n <- [1..]]
                series2 = map (toRational (exp (-x)) *) $ scanl (*) (toRational $ x**s / gamma(s+1)) [toRational x/n | n <- [toRational s+1 ..]]


-- |Special case of Kummer's confluent hypergeometric function: \s z -> M(1;s+1;z)
m_1_sp1 s z = convergingSum (scanl (*) 1 [z / x | x <- [s+1..]])

-- |Add a possibly-infinite sum until its value doesn't change anymore.
convergingSum xs = converge (scanl1 (+) xs)
    where
        converge []     = 0
        converge [x]    = x
        converge (x:rest@(y:_))
            | x == y    = y
            | otherwise = converge rest


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
        | n < 0         = error "factorial: n < 0"
        | n < nFacs     = facs V.! fromIntegral n
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

-- |Add a possibly-infinite sum until its value doesn't change anymore.
convergingSumRat absErr relErr xs = fromRational (converge (scanl1 (+) (map toRational xs)))
    where
        converge []     = 0
        converge [x]    = x
        converge (x:rest@(y:_))
            | abs(x-y) <= toRational absErr    = y
            | err x y <= toRational relErr    = y
            | otherwise = converge rest

err a b = abs (a-b) / max (abs a) (abs b)
