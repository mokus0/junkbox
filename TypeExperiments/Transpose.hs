{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances #-}
{-
 -	"Transpose.hs"
 -	(c) 2008 James Cook
 -}

module TypeExperiments.Transpose where

-- functor transposition...

class Transpose f g | f -> g where
        transpose :: f -> g

instance Transpose ([a],[b]) [(a,b)]
        where
                transpose = uncurry zip

instance Transpose [(a,b)] ([a],[b])
        where
                transpose = unzip
