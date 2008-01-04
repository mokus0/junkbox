{-# OPTIONS -fglasgow-exts #-}
{-
 -	"Quadrilaterals.hs"
 -	(c) 2007 James Cook
 -}

module Quadrilaterals where

class Shape a b sc | a -> b, a -> sc where
	scale :: a -> b -> b -> sc
	test :: a -> (b, b) -> Bool

data Square b = Square b b b
	deriving (Eq, Show)

data Box b = Box b b b b
	deriving (Eq, Show)

instance (Num b) => Shape (Square b) b (Box b)
	where
		scale sq x y = Box undefined undefined undefined undefined 
		test = undefined

instance (Num b) => Shape (Box b) b (Box b)
	where
		scale b x y = Box undefined undefined undefined undefined 
		test = undefined