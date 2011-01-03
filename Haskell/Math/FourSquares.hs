module Math.FourSquares where

-- Lagrange proved that this function returns a non-empty list for
-- all nonnegative integers.
-- http://en.wikipedia.org/wiki/Lagrange%27s_four-square_theorem

-- Given a number n, return 4 numbers a,b,c,d such that:
--      a <= b <= c <= d
-- and  a^2 + b^2 + c^2 + d^2 == n
fourSquares n = 
    [ (a,b,c,d)
    | a <- g 0 0,    let tot1 = sq a
    , b <- g a tot1, let tot2 = tot1 + sq b
    , c <- g b tot2, let tot3 = tot2 + sq c
    , d <- g c tot3, let tot4 = tot3 + sq d
    , n == tot4
    ] where 
        sq x = x*x
        g p q = takeWhile ((<= (n-q)) . sq) [p..]
