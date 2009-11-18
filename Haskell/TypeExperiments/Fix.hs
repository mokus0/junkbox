#!runhaskell
{-# OPTIONS -fglasgow-exts #-}
{-
 -	"Fix.hs"
 -	(c) 2007 James Cook
 -}

module TypeExperiments.Fix where

data ListF a b = NilF | ConsF a b
data TreeF a b = TipF | NodeF a b b

data Fix (s :: * -> * -> *) a = In {out :: s a (Fix s a)}

type List a = Fix ListF a
type Tree a = Fix TreeF a