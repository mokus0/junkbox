#!runhaskell
{-
 -	"SetMonad.hs"
 -	(c) 2007 James Cook
 -}

module SetMonad where

import Prelude hiding ((>>=), return, fail)
import Data.Set hiding (filter, map)

set >>= f = unions (set : (toList $ mapMonotonic f set))
return = singleton
fail _ = empty

i = fromList [1]
f x
	| (x - 1) %= 3	= fromList [(x - 1) `div` 3, 2*x]
	| otherwise		= singleton (2*x)

x %= y		= (x `mod` y) == 0

collatzDomain = iterate (>>= f) i

zipFilter op xs ys = map snd $ filter fst $ zip (zipWith op xs ys) xs

firstMissing xs = head (zipFilter (/=)  [0..] (xs ++ (repeat 0)))