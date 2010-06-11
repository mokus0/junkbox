{-# LANGUAGE ParallelListComp #-}
-- |Lanczos' approximation to the gamma function, as described at
-- http://en.wikipedia.org/wiki/Lanczos_approximation
-- (fetched 11 June 2010)
module Math.Gamma.Lanczos (gammaLanczosVar, gammaLanczosConst, p, cs) where

reflect gamma z
    | z > 0.5   = gamma z
    | otherwise = pi / (sin (pi * z) * gamma (1-z))

gammaLanczosVar g p = reflect gamma
    where
        gamma zp1
            | e > 0     = sqrt (2*pi) * e ** (z + 0.5) * exp (negate e) * a gam z
            | otherwise = error (concat ["gammaLanczosVar: invalid g (", show gam, ") for z (", show z, ")"])
            where 
                gam = g z
                e = z + gam + 0.5
                z = zp1 - 1
        
        a g z = 0.5 * p 0 g + sum [ p k g * num / denom | k <- [1..] | num <- fallingPowers z | denom <- risingPowers z]

gammaLanczosConst g cs = reflect gamma
    where
        gamma zp1
            | e > 0     = sqrt (2*pi) * e ** (z + 0.5) * exp (negate e) * a z
            | otherwise = error (concat ["gammaLanczos: invalid g (", show g, ") for z (", show z, ")"])
            where 
                e = z + g + 0.5
                z = zp1 - 1
        
        a z = head cs + sum [c / (z + k) | c <- tail cs | k <- [1..]]

cs g = undefined

p k g = sum [c (2*k+1) (2*a+1) * f a | a <- [0..k]]
        where
            k' = fromIntegral k
            f a = sqrt 2 / pi
                * gamma_n_plus_half a 
                * (a' + g + 0.5) ** (negate (a' + 0.5)) 
                * exp (a' + g + 0.5)
                where a' = fromIntegral a

            gamma_n_plus_half n = fromInteger (fac (fac (2 * n - 1)) `div` (2^n))
            fac n = product [1..n]

c 1 1 = 1
c 2 2 = 2
c i 1 = negate (c (i-2) 1)
c i j
    | i == j    = 2 * c (i-1) (j-1)
    | otherwise = 2 * c (i-1) (j-1) - c (i-2) j

{-# INLINE risingPowers #-}
risingPowers x = scanl1 (*) (iterate (1+) x)

{-# INLINE fallingPowers #-}
fallingPowers x = scanl1 (*) (iterate (subtract 1) x)
