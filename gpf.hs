

-- gpf: greatest prime factor.

gpf x = last (1: (factor x))

factor x = _factor x primes
	where
		_factor 0 _ = [0]
		_factor 1 _ = []
		_factor x (p:plist)
			| (x `mod` p > 0) = _factor x plist
			| otherwise = p : _factor (x `div` p) (p:plist)

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


-- functions for analyzing iterative cycles
-- leadin:  returns the non-cyclic leadin portion of a list.  may be infinite.
-- cycle:  returns (1 instance of) the cyclic part of a list.  if leadin is infinite, will never return.
-- cycles:  returns a list of all unique answers to "cycle fn x", where x is in startlist

-- leadIn fn start
-- cycle fn start
-- cycles fn startlist

-- goal:
-- f x = cycle (\y -> x + gpf y) 1