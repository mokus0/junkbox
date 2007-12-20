#!runhaskell
{-
 -	"/var/tmp/folders.501/TemporaryItems/untitled.hs"
 -	(c) 2007 James Cook
 -}

module Untitled where
import Data.List

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

maximize f xs = snd (maximumBy (\a b -> (fst a) `compare` (fst b)) $ zip (map f xs) xs)

maximize' f (m,[]) = (m,[])
maximize' f (m,x:xs) = (max m (f x), xs)