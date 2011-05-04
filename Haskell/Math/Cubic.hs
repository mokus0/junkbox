module Math.Cubic where

import Data.Complex

cbrt 0 = [0,0,0]
cbrt 1 = [1, (-0.5) :+ 0.5 * sqrt 3, (-0.5) :+ (-0.5) * sqrt 3]
cbrt x = do
    sgn <- cbrt 1
    return (sgn * x ** (1/3))

-- solve a cubic equation:
-- 
-- @x `elem` cubic a b c d@
--    ==>
-- @evalPoly (poly BE [a,b,c,d]) x ~= 0@
cubic 1 b c d = do
    let b2 = b*b; b3 = b*b2
        p = (3*c - b2)
        q = (b*(4.5*c - b2) - 13.5*d)
    u <- cbrt (q - sqrt (q^2 + p^3))
    return ((u - b - p / u) / 3)
cubic a b c d = cubic 1 (b/a) (c/a) (d/a)
