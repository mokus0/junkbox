#!runhaskell
{-
 -	"/var/tmp/folders.501/TemporaryItems/untitled.hs"
 -	(c) 2007 James Cook
 -}

module Untitled where
import Data.List
import qualified Data.Map as M

f x
	| x <= 1	= x
	| even x	= f (x `div` 2)
	| otherwise = f (3 * x + 1)

f' x
	| x <= 1	= x
	| even x	= x `div` 2
	| otherwise = 3 * x + 1

fix f x
	| f x == x		= x
	| otherwise		= fix f (f x)

trace f x
	| f x == x		= [x]
	| otherwise		= x : (trace f (f x))

itersTill p f x = iters 0 x
	where
		iters n x
			| p x	= n
			| True	= iters (n+1) (f x)

maximize f xs = snd  $ maximumBy (\a b -> (fst a) `compare` (fst b)) $ zip (map f xs) xs

maximize' f (m,[]) = (m,[])
maximize' f (m,x:xs) = seq m $ maximize' f (max m (f x), xs)


dist :: Integer -> Integer
dist 1 = 1
dist n = 1 + dist' (f' n)
	where dist' n = dists `genericIndex` n

dists :: [Integer]
dists = [dist n | n <- [0..]]

q1 x = case (x-1) `divMod` 3 of
		(y,0)	-> if (odd y) && y > 1 then return y else fail ""
		_	-> fail ""

q2 x = return (x*2)
	
q x = nub $ do
	x <- x
	(q1 x) ++ (q2 x)

foo = qR [2..999999] [1]

qR ys xs = if null ys' then sort xs' else qR ys' xs'
	where
		ys' = ys \\ xs'
		xs' = q xs


wordify 1   = "one"
wordify 2   = "two"
wordify 3   = "three"
wordify 4   = "four"
wordify 5   = "five"
wordify 6   = "six"
wordify 7   = "seven"
wordify 8   = "eight"
wordify 9   = "nine"
wordify 10  = "ten"

wordify 11  = "eleven"
wordify 12  = "twelve"
wordify 13  = "thirteen"
wordify 14  = "fourteen"
wordify 15  = "fifteen"
wordify 16  = "sixteen"
wordify 17  = "seventeen"
wordify 18  = "eighteen"
wordify 19  = "nineteen"

wordify 20  = "twenty"
wordify 30  = "thirty"
wordify 40  = "forty"
wordify 50  = "fifty"
wordify 60  = "sixty"
wordify 70  = "seventy"
wordify 80  = "eighty"
wordify 90  = "ninety"

wordify x
	| x >= 1000	= (wordify t) ++ " thousand" ++ if rest1 > 0 then ' ':(wordify rest1) else ""
	| x >= 100	= (wordify h) ++ " hundred" ++ if (rest2 > 0) then " and " ++ (wordify rest2) else ""
	| otherwise 	= (wordify (x-rest3)) ++ if rest3 > 0 then ' ':(wordify rest3) else ""
		where 
			(t,rest1) = x `divMod` 1000
			(h,rest2) = x `divMod` 100
			rest3 = x `mod` 10


fcycle d = fc (M.singleton 0 0) 0 1 d
fc seen x n d = case quotRem n d of
	(q,r) -> case M.lookup r seen of
		Just y	-> x - y
		Nothing	-> fc (seen') (x+1) (r*10) d
			where seen' = M.insert r x seen
	
	--if seen then ... else fcycle r d
	