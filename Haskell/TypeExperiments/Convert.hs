{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
{-
 -	"Convert.hs"
 -	(c) 2008 James Cook
 -}

module TypeExperiments.Convert where

class Convert a b where
	convert :: a -> b

class ConvertVia a b c | a b -> c

instance Convert Rational Integer where
	convert = round

instance (Integral a) => Convert a Integer where
	convert = toInteger

instance (Real a) => Convert a Rational where
	convert = toRational

instance (Num a) => Convert Integer a where
	convert = fromInteger

instance (Fractional a) => Convert Rational a where
	convert = fromRational

instance (Convert a b, Convert b c, ConvertVia a c b) => Convert a c where
	convert = (convert :: b -> c) . (convert :: a -> b)

instance ConvertVia Int a Integer
instance ConvertVia Float a Rational
instance ConvertVia Double a Rational