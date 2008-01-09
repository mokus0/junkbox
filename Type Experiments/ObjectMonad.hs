{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies #-}
{-
 -	"ObjectMonad.hs"
 -	(c) 2008 James Cook
 -}

module ObjectMonad where

infixl 8 #

class (Monad m) => ObjectMonad m
	where
		self :: m o
		(#) :: m o -> (o -> m o2) -> m o2

o ## f = o # return . f