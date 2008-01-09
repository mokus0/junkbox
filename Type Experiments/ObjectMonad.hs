{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies #-}
{-
 -	"ObjectMonad.hs"
 -	(c) 2008 James Cook
 -}

module ObjectMonad where

infixl 8 #

class (Monad m) => ObjectMonad o m | m -> o
	where
		self :: m o
		(#) :: (ObjectMonad o2 m2) => m o -> (o -> m o2) -> m2 o2

o ## f = o # return . f