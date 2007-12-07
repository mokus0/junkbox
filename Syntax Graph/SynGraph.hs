#!runhaskell
{-
 -	"SynGraph.hs"
 -	(c) 2007 James Cook
 -
 -	"It's full of jam..."
 -}

module SynGraph where

data Kind = Kind | KindFunc Kind Kind
	deriving (Eq, Show)

data Node = Node {
	name :: String,
	
	kind :: Kind
} deriving (Eq, Show)

