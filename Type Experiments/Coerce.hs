{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
{-
 -	"Coerce.hs"
 -	(c) 2008 James Cook
 -}

module Coerce where

class Coerce a b c | a b -> c where
	coerce :: a -> b -> (c,c)

instance (Integral a) => Coerce a Integer Integer
	where
		coerce x y = (toInteger x, y)

instance (Integral a) => Coerce Integer a Integer
	where
		coerce x y = (x, toInteger y)

instance Coerce a a a where	-- arr... i don't like the way ghc resolves (fails to resolve, that is) these...
	coerce = (,)

