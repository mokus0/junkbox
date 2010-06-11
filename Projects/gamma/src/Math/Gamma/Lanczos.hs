{-# LANGUAGE ParallelListComp #-}
-- |Lanczos' approximation to the gamma function, as described at
-- http://en.wikipedia.org/wiki/Lanczos_approximation
-- (fetched 11 June 2010)
module Math.Gamma.Lanczos where

reflect gamma z
    | z > 0     = gamma z
    | otherwise = pi / (sin (pi * z) * gamma (1-z))

gammaLanczos g ps = reflect gamma
    where
        gamma zp1
            | z + g + 0.5 > 0   = sqrt (2*pi) * (z + g + 0.5) ** (z + 0.5) * exp (negate (z + g + 0.5)) * a_g z
            | otherwise         = error (concat ["gammaLanczos: invalid g (", show g, ") for z (", show z, ")"])
            where z = zp1 - 1
        
        a_g z = undefined

ps g = undefined