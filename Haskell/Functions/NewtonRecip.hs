-- Newton-Raphson division: uses the Newton-Raphson method applied to "F(x) = 1/x + d" to find the reciprocal of d using only addition and multiplication.
module Functions.NewtonRecip (newtonRecip, newtonDiv) where

import Math.Root.Finder
import Math.Root.Finder.Newton

-- I'd like to believe that scale and unscale get compiled down to efficient
-- bit-shifts and such, but i kinda doubt it.

-- scale (n, x) = x * 2^n
scale :: (Int, Double) -> Double
scale (n, x) = encodeFloat mx (ex + n)
    where
        (mx, ex) = decodeFloat x

-- scale . unscale == id, except at -0 and +-NaN
unscale :: Double -> (Int, Double)
unscale x = (ex - e1 + 1, encodeFloat mx (e1 - 1))
    where
        (mx, ex) = decodeFloat x
        ( _, e1) = decodeFloat 1

newtonRecip 0 = 0/0
newtonRecip d = scale . newtonRecip' . unscale $ d
    where
        newtonRecip' :: (Int, Double) -> (Int, Double)
        newtonRecip' (n, d) = (-n, iterate step start !! 5)
            where
                t1 = 2.914213562373095 -- 3/2 + sqrt 2
                t2 = 2
                
                start = t1 - t2 * d
                step x = x * (2 - d * x)

newtonDiv x y = x * newtonRecip y

