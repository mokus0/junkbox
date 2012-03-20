module Math.Cubic where

import Data.Complex
import Math.Polynomial
import Test.QuickCheck

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

eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')

x ~= y     = err x y < (eps ** (1/3))
err    0 0 = 0
err    x y = min (absErr x y) (relErr x y)
absErr x y = magnitude (x - y)
relErr x y = absErr x y / max (magnitude  x) (magnitude  y)
rootErr f x = err y 0 / max 1 (magnitude y')
    where (y, y') = evalPolyDeriv f x

-- this does not always hold, but the majority of the time it does.
-- The cases where it fails tend to have extremely large derivatives
-- at the roots.
prop_accuracy a b c d =
    let ps = [a,b,c,d]
        f  = poly BE ps
        rs = cubic a b c d
     in any (/= 0) ps
    ==> all ((< sqrt eps) . rootErr f) rs
