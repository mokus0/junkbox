module Math.ContinuedFractions where

import Data.Ratio

{------------------ Some interesting continued fractions ------------------}
	-- precise GCF for pi
gcf_pi = (0,4) : gcf_4_pi
gcf_4_pi = zip [1,3..] (map (^ 2) [1..])

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


cf_e = 2 : cf_e_frac
cf_e_frac = 1 : 2 : 1 : _extend cf_e_frac
	where _extend (x:y:z:xs) = x : (2 + y) : z : _extend xs


{----------------------------- some old stuff -----------------------------}
cfToR :: (Integral a) => [a] -> Rational
cfToR [] = 0;
cfToR [x] = toRational x;
cfToR (x:xs) = (toRational x) + (1 / cfToR xs)

	-- floating point rounding errors pop up here very quickly :(
	-- (cf pi) is accurate to 13 places, though, which is better than I expected
cf :: (RealFrac a, Integral b) => a -> [b]
cf 0 = []
cf x = (_int x) : (cf (_recip (_frac x)))
	where
		_int x = fromInteger (floor x)
		_frac x = x - (_int x)
		_recip x = if (x == 0) then 0 else recip x

gcfToR :: Integral a => [(a,a)] -> Rational
gcfToR [] = 0;
gcfToR [(a,b)] = toRational a
gcfToR ((a,b):xs) = (toRational a) + ((toRational b) / (gcfToR xs))

	-- cfNormalize: take a cf that may have zeroes and fix it
cfNormalize :: (Integral a) => [a] -> [a]
cfNormalize [] = [0]
cfNormalize [a] = [a]
cfNormalize (a:0:a2:rest) = cfNormalize ((a + a2) : rest)
cfNormalize (a:as) = a : (cfNormalize as)


-- cfDiv :: (Integral a) => [a] -> a -> [a]
-- cfDiv (a:as) b = cfNormalize (_cfDiv (a:as) b)

-- _cfDiv :: (Integral a) => [a] -> a -> [a]
-- _cfDiv