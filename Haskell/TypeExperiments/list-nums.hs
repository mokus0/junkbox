{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
import Monad
import System

		-- Haskell kicks ass.
		

instance (Num a) => Num [a]
	where
		(+) = liftM2 (+)
		(*) = liftM2 (*)
		(-) = liftM2 (-)
		negate = liftM negate
		abs = liftM abs
		signum = liftM signum
		fromInteger = return . fromInteger


instance (Fractional a) => Fractional [a]
	where
		(/) = liftM2 (/)
		recip = liftM recip
		fromRational = return . fromRational

instance (Floating a) => Floating [a]
	where
		pi = return pi
		exp = liftM exp
		sqrt x = x >>= (\y -> [sqrt y, negate $ sqrt y])
		log = liftM log
		(**) = liftM2 (**)
		logBase = liftM2 logBase
		sin = liftM sin
		tan = liftM tan
		cos = liftM cos
		asin = liftM asin
		atan = liftM atan
		acos = liftM acos
		sinh = liftM sinh
		tanh = liftM tanh
		cosh = liftM cosh
		asinh = liftM asinh
		atanh = liftM atanh
		acosh = liftM acosh

instance (Real a) => Real [a]
	where
		toRational = toRational . head

instance (Integral a) => Integral [a]
	where
		quot = liftM2 quot
		rem = liftM2 rem
		div = liftM2 div
		mod = liftM2 mod
		quotRem x y = (x `quot` y, x `rem` y)
		divMod x y = (x `div` y, x `mod` y)
		toInteger = toInteger . head

replicate2 :: Integer -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : (replicate2 (n - 1) x)

toInt :: Integer  -> Int
toInt x = if (x > 0) then _toInt x else (-1) * (_toInt (negate x))
	where
		_toInt x = length (replicate2 x ())

instance (Num a, Integral a, Ord a, Real a) => Enum a
	where
		toEnum = fromInteger . toInteger
		fromEnum = toInt . toInteger
		succ = (+1)
		pred = (subtract 1)
		enumFrom = iterate succ
		enumFromTo x y = takeWhile (<= y) (enumFrom x)
		enumFromThenTo x y z = takeWhile (<= z) (iterate (+diff) x)
			where diff = y - x
			
			

--Early haskell experimentation, with some later updates.

-- congruent: given a modulus and a (ordered) list of numbers less than the
-- modulus, generate an infinite (ordered) list of numbers congruent to the
-- numbers in the list modulo the given modulus
congruent modulus list = _congruent 0
	where _congruent x = (map (+x) list) ++ (_congruent $! (x + modulus))

wheel30 = congruent 30 [1,7,11,13,17,19,23,29]

skipTo x (y:ys) = if (y > x) then (y:ys) else skipTo x ys

primes = 2 : 3: 5 : (filter isPrime (skipTo 3 wheel30))

isPrime 0 = False
isPrime 1 = False
isPrime x = not (isComposite x)
isComposite x = any (\y -> x `mod` y == 0) (takeWhile (\y -> y*y <= x) primes)


factorNM x n = ifThenElse (x `remM` n `gte` (return 1)) (return 0) ((return 1) `plus` factorNM (x `quotM` n) n)
	where
		remM = liftM2 rem
		quotM = liftM2 quot
		gte = liftM2 (>=)
		plus = liftM2 (+)
		ifThenElse = liftM3 _ifThenElse
			where
				_ifThenElse x y z = if x then y else z

-- Non-computational part... read a number n, print the nth prime

nthPrime n = primes !! (n-1)

