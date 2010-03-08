module NR.Ch5.S6 
    ( quadratic, complexQuadratic
    ) where

import Data.Complex

linear 0 b = []
linear a b = [negate b / a]

quadratic 0 b c = linear b c
quadratic a b c = case disc `compare` 0 of
    GT -> (q/a) : [c/q | q /= 0]
    EQ -> [negate b / (2 * a)]
    LT -> []
    where
        disc = b*b - 4*a*c
        {-# NOINLINE q #-}
        q = (-0.5) * (b + signum b * sqrt disc)

complexQuadratic 0 b c = linear b c
complexQuadratic a b c 
    | disc == 0 = linear (2*a) b
    | otherwise = (q/a) : [c/q | q /= 0]
    where
        disc = b*b - 4*a*c
        {-# NOINLINE q #-}
        q = (-0.5) * (b + setSign (sqrt disc))
        
        setSign z   | realPart (conjugate b * z) >= 0   = z
                    | otherwise                         = negate z


cubic 0 a b c = quadratic a b c
cubic 1 a b c
    | r^2 < q^3 = let theta = acos (r * q**(-3/2))
                      foo t = (-2) * sqrt q * cos ((theta + 2*t*pi) / 3) - (a / 3)
                   in map foo [0,1,-1]
    | otherwise = error "cubic: write me!"
    where
        q = (a^2 - 3*b) / 9
        r = (2*a^3 - 9*a*b + 27*c) / 54
cubic a b c d = let s = recip a in cubic 1 (b*s) (c*s) (d*s)

complexCubic = error "complexCubic: write me!"



-- a few sanity tests

-- evaluate a polynomial, const coeff first
evalPoly p x = go p
    where
        go []       = 0
        go (c:cs)   = c + x * go cs

isZero abs p x = abs y / big < 1e-14
    where
        y = evalPoly p x
        big = sum (map abs p) ^ 2

realQuadratic_test    a b c = all (isZero abs       [c,b,a]) (quadratic        a b c)
complexQuadratic_test a b c = all (isZero magnitude [c,b,a]) (complexQuadratic a b c)

realCubic_test    a b c d = all (isZero abs       [d,c,b,a]) (cubic        a b c d)
complexCubic_test a b c d = all (isZero magnitude [d,c,b,a]) (complexCubic a b c d)
