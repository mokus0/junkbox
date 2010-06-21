{-# LANGUAGE ParallelListComp #-}
-- |Lanczos' approximation to the gamma function, as described at
-- http://en.wikipedia.org/wiki/Lanczos_approximation
-- (fetched 11 June 2010).
-- 
-- Constants to be supplied by user.  There is a file \"extras/LanczosConstants.hs\"
-- in the source repository that implements a technique by Paul Godfrey for
-- calculating the coefficients.  It is not included in the distribution yet 
-- because it makes use of a linear algebra library I have not yet released 
-- (though I eventually intend to).
module Math.Gamma.Lanczos (gammaLanczos, lnGammaLanczos, reflect) where

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
