module Math.SquareRoot where

improve x est = 0.5 * (est + x/est)

absConverge eps (x:y:rest)
    | abs (x-y) <= eps  = y
    | otherwise         = absConverge eps (y:rest)

sqrt1 x = absConverge 1e-15 (iterate (improve x) 1)

relConverge eps (x:y:rest)
    | abs(x-y) <= eps * abs y   = y
    | otherwise                 = relConverge eps (y:rest)

sqrt2 x = relConverge 1e-15 (iterate (improve x) 1)