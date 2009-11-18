primes :: Integral a => [a]
primes = 2 : (filter isPrime [3,5..])

isPrime, isComposite :: Integral a => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = not (isComposite x)
isComposite x = any (\y -> x `mod` y == 0) (takeWhile (\y -> y*y <= x) primes)

factor :: Integral a => a -> [(a,a)]
factor x = _merge (_factor x primes)
	where
		_factor 1 _ = []
		_factor x (p:plist) | (x `mod` p > 0) = _factor x plist
				    | otherwise = (p,1) : _factor (x `div` p) (p:plist)
		_merge ((f1,e1):(f2,e2):fs) | f1 == f2 = _merge ((f1,e1 + e2):fs)
					    | otherwise = (f1,e1) : _merge ((f2,e2):fs)
		_merge x = x

unfactor :: Integral a => [(a,a)] -> a
unfactor [] = 1
unfactor ((f,e):fs) = f^e * unfactor fs
