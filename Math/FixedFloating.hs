{-# LANGUAGE ViewPatterns, RankNTypes #-}
module Math.FixedFloating where

import Math.Converge
import Math.GCF

import TypeExperiments.WithResolution

import Data.Fixed
import Data.StateRef
import Control.Monad.ST
import Data.Ratio
import Data.List

evalGcfFixed :: HasResolution r => [(Rational, Rational)] -> Fixed r
evalGcfFixed gcf = x
    where
        r = resolution (f x)
        f :: f a -> a
        f = undefined
        x = fromRational (evalGcf (1 % (2*r)) gcf) 

instance HasResolution r => Floating (Fixed r) where
    pi = converge (gcfToRs gcf_pi)
    tan x = evalGcfFixed (gcf_tan (toRational x)) -- overconverge 1 (gcfToRsFrac (gcf_tan x))
    
    sqrt x = sqrtBy (==) x
    
    log x = evalGcfFixed (gcf_ln (toRational x))
    
    -- fails for n=2, underconverges for numbers with large num/denoms
    exp n = case n `compare` 0 of
        LT  -> recip (exp (negate n))
        EQ  -> 1
        GT  -> evalGcfFixed (gcf_exp2x_div_y (fromInteger x) (fromInteger y))
        where
            n' = toRational n
            x = numerator n'
            y = 2 * denominator n'
    sinh x = 0.5* (exp x - exp (-x))
    cosh x = 0.5* (exp x + exp (-x))
    tanh x = (\y -> (y-1)/(y+1)) (exp (x + x))
    asinh x = log (x + sqrt (x^2 + 1))
    acosh x = log (x + sqrt (x^2 - 1))
    atanh x = 0.5 * log ((1+x)/(1-x))

testC :: (Floating a) => (forall x. Floating x => x) -> a
testC x = realToFrac (x :: Double) - x

testF :: (Real a, Floating a) => (forall x. Floating x => x -> x) -> Double -> a
testF f x = f (realToFrac x) - realToFrac (f x)

series_ln_x_div_xm1 x = zipWith (/) (iterate (*recip x) (recip x)) [1..] 
ln_x_div_xm1_by (==) x = convergeBy (==) (scanl1 (+) (series_ln_x_div_xm1 x))
ln_x_div_xm1 x = converge (scanl1 (+) (series_ln_x_div_xm1 x))
ln x = ln_x_div_xm1 (x/(x-1))

ln_2 :: Floating a => a
ln_2 = log 2

agmStep  (x, y) = (0.5 * (x+y), sqrt x * sqrt y)
agmBy (==) out x y = convergeIntervalsBy (==) out (iterate agmStep (x,y))
agm x y = agmBy (==) const x y

agmRStep (x, y) = (2 * x*y / (x+y), sqrt (x*y))
agmRBy (==) out x y = convergeIntervalsBy (==) out (iterate agmRStep (x,y))
agmR x y = agmRBy (==) const x y

ln_hp m x = 0.5 * pi * agmR 1 s - fromIntegral m * ln_2
    where
        s = x * 2^(m-2)

sqrtBy (==) a = a * invSqrtBy (==) a
invSqrtBy (==) a = go x_0
    where
        x_0 = recip a
        go x_i
            | eps_i == 0    = x_i
            | otherwise     = go x_ip1
            where
                x_ip1 = x_i - 0.5 * x_i * (eps_i - 0.75 * eps_i * eps_i)
                eps_i = a * x_i * x_i - 1

