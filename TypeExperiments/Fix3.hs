{-
 -      ``Fix3''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeFamilies,
        UndecidableInstances
  #-}

module TypeExperiments.Fix3 where

-- doesn't actually work; tries to compute the "whole" type, causing the
-- compiler to loop until it runs out of stack
type family Fix (f :: * -> *) :: *
type instance Fix f = f (Fix f)

data List x f = Nil | Cons x f
	deriving (Eq, Ord, Show, Read)
type ListF x = Fix (List x)
