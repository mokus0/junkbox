module NR.Ch6.S2
    ( gammp, gammq
    ) where

import NR.Ch4.S6
import NR.Ch5.S2 (CF(..), evalCF, expandInPlaceCD, expandInPlace)
import NR.Ch6.S1
import qualified Data.Vector.Unboxed as V

aSwitch     = 100 :: Int
fpMin       = 2.2250738585072014e-308 / eps :: Double   -- constant here is smallest positive normalized Double
eps         = 2.220446049250313e-16 :: Double

gammp a x
    | x < 0                 = error "gammp: x < 0"
    | a <= 0                = error "gammp: a <= 0"
    | x == 0                = 0 
    | truncate a >= aSwitch = gammpapprox a x True
    | x < a + 1             = gser a x
    | otherwise             = 1 - gcf a x

gammq a x
    | x < 0                 = error "gammq: x < 0"
    | a <= 0                = error "gammq: a <= 0"
    | x == 0                = 1
    | truncate a >= aSwitch = gammpapprox a x False
    | x < a + 1             = 1 - gser a x
    | otherwise             = gcf a x

gser a x = go (recip a) (recip a) a
    where
        gln = gammln a
        
        go sum del ap
            | abs del < abs sum * eps
                = sum * exp (negate x + a * log x - gln)
            | otherwise
                = go sum' del' ap'
                where
                    ap'  = ap + 1
                    del' = del * x / ap'
                    sum' = sum + del'
        

gcf a x = exp (negate x + a * log x - gln) * h
    where 
        gln = gammln a
--        h = recip . flip evalCF eps $ GCF [(x + 2 * n - 1 - a, n * (n - a)) | n <- [1..]]
        h = expandInPlaceCD fpMin eps . GCF $ zip bs as
        as = 1 : [negate i * (i - a) | i' <- [1..], let i = fromInteger i']
        bs = 0 : iterate (+2) (x+1-a)

ngau = 18 :: Int
gauleg36 = gauleg 0 2 (ngau * 2) 1e-15 :: GaussQ V.Vector Double

gammpapprox a x psig = go 0 0
    where
        a1 = a - 1
        lna1 = log a1
        sqrta1 = sqrt a1
        gln = gammln a
        
        xu  | x > a1    =        max (a1 + 11.5 * sqrta1) (x + 6 * sqrta1)
            | otherwise = max 0 (min (a1 - 7.5  * sqrta1) (x - 5 * sqrta1))
        
        go j sum
            | j >= ngau = end sum
            | otherwise = go (j+1) (sum + w * exp ((a1 - t) + a1 * (log t - lna1)))
            where 
                y = abscissas gauleg36 V.! j
                w = weights   gauleg36 V.! j
                t = x + (xu - x) * y

        end sum 
            | psig      = if ans >  0 then 1 - ans else negate ans
            | otherwise = if ans >= 0 then ans     else 1 + ans
            where
                ans = sum * (xu - x) * exp (a1 * (lna1 - 1) - gln)
