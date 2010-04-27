{-# LANGUAGE ParallelListComp #-}
module NR.Ch4.S4 where

import NR.Ch4.S3

midpnt f a b = go 0 ((b-a) * f (0.5 * (a+b)))
    where
        genXs x0 del ddel = x0 : genXs (x0 + ddel) ddel del
        go n s = s : go (n+1) s'
            where
                iters = 3^n
                tnm = fromIntegral iters
                del = (b-a) / (3 * tnm)
                ddel = del+del
                x0 = a + 0.5 * del
                xs = genXs x0 del ddel
                ys = [ f x | x <- take (2 * iters) xs ]
                s' = s / 3 + del * sum ys


-- TODO: generalize these intergal-transform rules to apply to all quadratures
midinf f a b
    | signum a * signum b >= 0
        = midpnt f' a' b'
    | otherwise
        = error "midinf: a and b must have the same sign"
    where 
        a' = recip b
        b' = recip a
        f' x = f (recip x) / (x*x)

midsql f a b = midpnt f' a' b'
    where
        a' = 0
        b' = sqrt (b-a)
        f' x = 2 * x * f (a + x*x)

midsqu f a b = midpnt f' a' b'
    where
        a' = 0
        b' = sqrt (b-a)
        f' x = 2 * x * f (b - x*x)

midexp f a b = midpnt f' a' b'
    where
        a' = 0
        b' = exp (negate a)
        f' x = f (negate (log x)) / x

qromo f a b eps = qromo' midpnt f a b eps
qromo' midpntrule f a b eps = converge (romb midpntrule (1/9) jmax k f a b)
    where
        jmax = 14
        k = 5
        
        converge ((x, dy):xs) 
            | abs dy <= eps * abs x     = (x, dy)
            | otherwise                 = converge xs
        converge _ = error "qromb: too many steps"
