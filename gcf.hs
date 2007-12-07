import Data.Ratio

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