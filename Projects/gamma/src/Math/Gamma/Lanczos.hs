{-# LANGUAGE 
        FlexibleContexts,
        ParallelListComp
  #-}
-- |Lanczos' approximation to the gamma function, as described at
-- http://en.wikipedia.org/wiki/Lanczos_approximation
-- (fetched 11 June 2010)
module Math.Gamma.Lanczos (gammaLanczos, lnGammaLanczos, reflect, cs) where

import NR.Ch1.S4
import NR.Ch1.S4.Alias

{-# INLINE reflect #-}
reflect gamma z
    | z > 0.5   = gamma z
    | otherwise = pi / (sin (pi * z) * gamma (1-z))

{-# INLINE gammaLanczos #-}
gammaLanczos g cs zp1
    | x > 0     = sqrt (2*pi) * x ** (zp1 - 0.5) * exp (negate x) * a cs z
    | otherwise = error (concat ["gammaLanczos: invalid g (", show g, ") for z (", show z, ")"])
    where
        x = zp1 + (g - 0.5)
        z = zp1 - 1

{-# INLINE lnGammaLanczos #-}
lnGammaLanczos g cs zp1
    | x > 0     = log (sqrt (2*pi)) + log x * (zp1 - 0.5) - x + log (a cs z)
    | otherwise = error (concat ["lnGammaLanczos: invalid g (", show g, ") for z (", show z, ")"])
    where 
        x = zp1 + (g - 0.5)
        z = zp1 - 1

{-# INLINE a #-}
a cs z = head cs + sum [c / (z + k) | c <- tail cs | k <- [1..]]

cs g n = vectorToList (applyRat dbc f)
    where
        applyRat :: (Real t, Fractional t) => IMatrix Rational -> IVector t -> IVector t
        applyRat m v = fromRatVec (apply m (toRatVec v))
        fromRatVec :: (Vector v t, Fractional t) => IVector Rational -> v t
        fromRatVec = convertByV fromRational
        toRatVec :: (Vector v t, Real t) => v t -> IVector Rational
        toRatVec   = convertByV toRational
        
        dbc = dbcMat n
        f = fVec g n

dbcMat n = multRat d (multRat b c)
    where
        multRat :: (Real a, Matrix m1 a, Real b, Matrix m2 b) => m1 a -> m2 b -> IMatrix Rational
        multRat = multiplyWith sum (\d b -> toRational d * toRational b)
        
        d = dMat n
        b = bMat n
        c = cMat n

fVec :: (Floating b, Vector v b) => b -> Int -> v b
fVec g n = vector n f
    where
        f a = sqrt (2 / pi)
            * product [fromIntegral i - 0.5 | i <-[1..a]]
            * exp (a' + g + 0.5)
            / (a' + g + 0.5) ** (a' + 0.5)
            where a' = fromIntegral a

cMat :: Int -> IMatrix Rational
cMat n = matrix n n m
    where
        m 0 0 = 1/2
        m i j = fromInteger (c (2*i) (2*j))
        
        c 0 0 = 1
        c 1 1 = 1
        c i 0 = negate (c (i-2) 0)
        c i j
            | i == j    = 2 * c (i-1) (j-1)
            | i > j     = 2 * c (i-1) (j-1) - c (i-2) j
            | otherwise = 0

dMat :: Int -> IAlias Mat Integer
dMat n = AsDiag (IVec (ivector n dFunc)) 0
    where
        dFunc    0  = 1
        dFunc (i+1) = negate (factorial (2*i+2) `div` (2 * factorial i * factorial (i+1)))
        factorial n = product [1..toInteger n]

bMat :: Int -> IMatrix Integer
bMat n = matrixFromList bList
    where
        bList = take n . map (take n) $
            repeat 1 : 
            [ replicate i 0 ++ bicofs (negate (toInteger i*2))
            | i <- [1..]
            ]
            
        
        bFunc 0 _ = 1
        bFunc i j
            | i > j = 0
        bFunc i j = bicofs (toInteger (2 * j - 1)) !! i
        
        bicofs x = go x 1 1
            where
                go num denom x = x : go (num+signum num) (denom+signum denom) (x * num `div` denom)

-- 
-- p g k = sum [c (2*k+1) (2*a+1) * f a | a <- [0..k]]
--         where
--             k' = fromIntegral k
--             f a = 
{-# INLINE risingPowers #-}
risingPowers x = scanl1 (*) (iterate (1+) x)

{-# INLINE fallingPowers #-}
fallingPowers x = scanl1 (*) (iterate (subtract 1) x)
