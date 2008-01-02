{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

newtype Fix f a = Fix (f (Fix f) a)
	deriving (Eq, Ord, Read, Show)

data List f a = Nil | Cons a (f a)
	deriving (Eq, Ord, Show, Read)
type ListF = Fix List

cons a b = Fix (Cons a b)
nil = Fix Nil

data Tree l f a = Tip | Tree a (l (f a))
	deriving (Eq, Ord, Show, Read)
type TreeF = Fix (Tree [])
