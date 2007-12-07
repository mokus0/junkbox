#!runhaskell
{-
 -	"Data/Monoid/Maybe.hs"
 -	(c) 2007 James Cook
 -}

module Data.Monoid.Maybe where

import Data.Monoid

data First a = First { getFirst :: Maybe a }
	deriving (Eq, Show)

instance Monoid (First a)
	where
		mempty = First Nothing
		mappend (First Nothing) x = x
		mappend x@(First (Just _)) _ = x

first :: [Maybe a] -> Maybe a
first ms = getFirst (mconcat $ map First ms)
