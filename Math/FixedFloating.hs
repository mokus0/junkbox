{-# LANGUAGE ViewPatterns, RankNTypes #-}
module Math.FixedFloating where

import TypeExperiments.WithResolution
import Data.Fixed
import Data.StateRef
import Control.Monad.ST
import Data.Ratio
import Data.List

	-- precise GCF for pi
gcf_4_pi = zip [1,3..] (map (^ 2) [1..])
gcf_pi = (0,4) : gcf_4_pi
gcf_tan x = zip (0:[1,3..]) (x : repeat (negate (x^2)))

gcf_exp x = zip (1:1:map (x+) [2..]) (x:map (negate x*)[1..])

gcf_exp2x_div_y x y = zip (1 : y-x : map (*y) [3,5..]) (2*x : repeat (x^2))
gcf_ln_1p_2x_div_y x y = zip 
    (0   : map (* (y+x)) [1,3..])
    (2*x : map (\n -> negate (n^2 * x^2)) [1..])
    -- (intersperse 1 (0: map (*y) [1,3..])) 
    -- (2*x : concatMap (replicate 2) (iterate (+x) x))

gcf_ln x = gcf_ln_1p_2x_div_y (n-d) (d+d)
    where
        n = fromInteger (numerator x)
        d = fromInteger (denominator x)

-- gcf_pi = zip (3:repeat 6) (map (^2) [1,3..])

cf_e = 2 : cf_e_frac
cf_e_frac = 1 : 2 : 1 : _extend cf_e_frac
	where _extend (x:y:z:xs) = x : (2 + y) : z : _extend xs

skewSnd a0 []             bn = [(bn,a0)]
skewSnd a0 ((b0,a1):rest) bn = (b0, a0) : skewSnd a1 rest bn

scanl2 :: (y -> y -> x -> y)
          -> y -> y -> [x] -> [y]
scanl2 f = go
    where
        go s t []     = [s,t]
        go s t (x:xs) = s : go t (f s t x) xs

stepA aa1 aa2 (b,a) = b * aa2 + a * aa1
stepB bb1 bb2 (b,a) = b * bb2 + a * bb1

tailSkewSnd []              end = error "tailSkewSnd: empty list"
tailSkewSnd ((b0,a1):rest)  end = go a1 rest
    where
        go a1 [] = [(end, a1)]
        go a1 ((b1,a2):rest) = (b1, a1) : go a2 rest

expandA gcf@((b0,_):_) = scanl2 stepA 1 b0 (tailSkewSnd gcf 1)
expandB gcf            = scanl2 stepB 0 1  (tailSkewSnd gcf 1)

expand gcf = zipWith divide (expandA gcf) (expandB gcf)
    where divide a b = fromIntegral a / fromIntegral b
expandFrac gcf = zipWith (/) (expandA gcf) (expandB gcf)
expandReal gcf = zipWith divide (expandA gcf) (expandB gcf)
    where divide a b = toRational a / toRational b

evalGcf :: Real a => Rational -> [(a,a)] -> Rational
evalGcf eps gcf = head (convergeBy (~=) (drop 2 (expandReal gcf)))
    where
        x ~= y      = (abs (x-y) < eps)

expandInPlace gcf = go (fromIntegral b0) 1 1 0 as (tail bs)
    where
        (bs@(b0:_), as@(a1:_)) = unzip gcf
        
        go aa_j aa_jm1 1 bb_jm1 (a_j : as) (b_j : bs)
            | aa_scaled == aa_j = aa_scaled
            | bb /= 0           = go (aa * r_bb) (aa_j * r_bb) 1 r_bb as bs
            | otherwise         = error "bb == 0"
                where
                    a = fromIntegral a_j
                    b = fromIntegral b_j
                    
                    aa_scaled = aa * r_bb
                    aa = b * aa_j + a * aa_jm1
                    bb = b        + a * bb_jm1
                    r_bb = recip bb

-- does *not* work in fixed point
expandInPlaceCD tiny eps ((0,a1):rest) = a1 / expandInPlaceCD tiny eps rest
expandInPlaceCD tiny eps gcf = go b0 0 b0 as (tail bs)
    where
        (bs@(b0:_), as@(a1:_)) = unzip gcf
        
        go c_jm1 d_jm1 f_jm1 (a_j : as) (b_j : bs)
            | abs (delt - 1) < eps    = f_j
            | otherwise = go c_j d_j f_j as bs
            where
                c_j = b_j + a_j // c_jm1
                d_j = recip' (b_j + a_j * d_jm1)
                delt = c_j * d_j
                f_j = f_jm1 * delt
                
                big = recip tiny
                recip' x@0 = big * signum x
                recip' x = recip x
                
                x // y@0 = x * big * signum y
                x // y = x / y

converge :: Eq a => [a] -> a
converge = head . convergeBy (==)

convergeBy (==) [] = error "convergeBy: empty list"
convergeBy (==) (x:xs) = go x xs
    where
        go x [] = [x]
        go x zs@(y:ys)
            | x == y    = zs
            | otherwise = go y ys

overconverge n = head . overconvergeBy (==) n
overconvergeBy (==) n (x:xs) = go 0 x xs 
    where
        go i x [] = [x]
        go i x zs@(y:ys)
            | x == y    = if i >= n
                then zs
                else go (i+1) y ys
            | otherwise = go 0 y ys

gcfToRs     gcf = drop 2 (expand gcf)
gcfToRsFrac gcf = drop 2 (expandFrac gcf)

cfToRs cf = gcfToRs (cfToGcf cf)
cfToGcf cf = [(b, 1) | b <- cf]

gcfToCf :: (Real t) => [(t, t)] -> [Rational]
gcfToCf ((b0,a1):(b1,a2):rest) = go (toRational b0) b1 (toRational a1) a2 rest

    where go b0 b1 a1 a2 ((b2,a3):rest) = b0 : go (toRational b1*s) b2 (toRational a2*s) a3 rest  -- gcfToCf ((b1 * s, a2 * s):rest)
            where
                s = recip a1

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

convergeIntervalsBy (==) out = go
    where
        go ((a,b):rest)
            | a == b    = out a b
            | otherwise = go rest

convergeIntervals xs = convergeIntervalsBy (==) const

agmStep (x, y) = (0.5 * (x+y), sqrt (x*y))

agmBy (==) out x y = convergeIntervalsBy (==) out (iterate agmStep (x,y))
agm x y = agmBy (==) const x y

ln_hp m x = pi / (2 * agm 1 (4/s)) - fromIntegral m * ln_2
    where
        s = x * 2^m