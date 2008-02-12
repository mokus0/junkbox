{-# OPTIONS -fth -fglasgow-exts #-}
{-
 -	"bar.hs"
 -	(c) 2008 James Cook
 -}

module Bar where

import Prelude hiding ((+), (*))
import qualified Fmap

class Product a b c | a b -> c
        where
                (*) :: a -> b -> c

class CoProduct a b c | a b -> c
        where   (+) :: a -> b -> c

infixl 7 *
infixl 7 :*:
data a :*: b = a :*: b
        deriving (Eq, Show)

infixl 6 +
infixl 6 :+:
data a :+: b 
        = InL a
        | InR b
        deriving (Eq, Show)

data a :^: b
        = Func (b -> a)

instance Product (a -> a') (b -> b') ((a :*: b) -> (a' :*: b'))
        where
                (*) = $(Fmap.fmap ''(:*:))

instance CoProduct (a -> a') (b -> b') ((a :+: b) -> (a' :+: b'))
        where
                (+) = $(Fmap.fmap ''(:+:))

