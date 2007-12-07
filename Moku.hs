module Moku
  where

x = 5
y = (6, "Hello")
z = x * fst y

roots [a, b, c] =
	[
		(-b + det) / (2 * a),
		(-b - det) / (2 * a)
	]
	where
		det = sqrt (b*b - 4*a*c)

fib 0 = 0
fib 1 = 1
fib n = if n > 1
	then fib (n - 1) + fib (n - 2)
	else fib (n + 2) - fib (n + 1)

xor True = not
xor False = id

join :: [a] -> [[a]] -> [a]
join _ [x] = x
join j (x:xs) = x ++ j ++ (join j xs)
join _ [] = []

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

first _ [] = Nothing
first p (x:xs) = if p x
	then Just x
	else first p xs

unmaybe x Nothing = x
unmaybe _ Just x = x

-- primes = 2 : map primeafter primes
--	where primeafter x = unmaybe 0 first isPrime [x+1..]
--	      isPrime x = 