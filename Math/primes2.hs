
-- Early haskell experimentation, with some later updates.

module Math.Main where

import System


-- congruent: given a modulus and a (ordered) list of numbers less than the
-- modulus, generate an infinite (ordered) list of numbers congruent to the
-- numbers in the list modulo the given modulus
congruent modulus list = _congruent 0
	where _congruent x = (map (+x) list) ++ (_congruent $! (x + modulus))

wheel30 = congruent 30 [1,7,11,13,17,19,23,29]

skipTo x (y:ys) = if (y > x) then (y:ys) else skipTo x ys

primes = 2 : 3: 5 : (filter isPrime (skipTo 3 wheel30))

-- x <^n y : x^n <= y
x <^ n = \y -> ((x^n) <= y)
x >^ n = \y -> (x >= y^2)

-- x %= y: "x divides y"
x %= y = (y `rem` x) == 0

isPrime 0 = False
isPrime 1 = False
isPrime x = x > 0 && not (isComposite x)
isComposite x = any (\y -> x `mod` y == 0) (takeWhile (x >^2) primes)
divisors x = filter (%= x) (takeWhile (x >=) [1..])

highlyComposite 1 = True
highlyComposite x = length (divisors x) > (maximum (map (length . divisors) [1..(x-1)]))

hcns = filter highlyComposite [1..]

primeGaps = zipWith (-) (tail primes) primes

primorial 1 = 1
primorial 2 = 2
primorial x = foldr1 (*) (takeWhile (<=x) primes)

primorials = 1 : (scanl1 (*) primes)

-- Non-computational part... read a number n, print the nth prime

nthPrime n = primes !! (n-1)

main = do
	args <- getArgs
	let n = read (args !! 0)
	print (nthPrime n)
	

-- 

n `divides` x = (x `rem` n) == 0
factorN n x = if n `divides` x then 1 + factorN n (x `quot` n) else 0
factor x = _factor primes x
	where
		_factor _ 0 = []
		_factor _ 1 = []
		_factor _ (-1) = []
		_factor (n:ns) x = f : (_factor ns (x `div` (n^f)))
			where
				f = factorN n x 

unfactor ns = foldl (*) 1 (zipWith (^) primes ns)
