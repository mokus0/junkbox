{-
 -      ``Fold''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeFamilies,
        RankNTypes
  #-}

module Fold where

type family FoldSig t a :: *
class Fold t where
        fold :: forall a. FoldSig t a -> t -> a

type instance FoldSig [x] a = (a, x -> a -> a)
instance Fold [x] where
        fold (nil, cons) = foldr cons nil

-- some small tuples...
type instance FoldSig (a, b) x = (a -> b -> x)
instance Fold (a,b) where
        fold = uncurry

type instance FoldSig (a, b, c) x = (a -> b -> c -> x)
instance Fold (a,b,c) where
        fold f (a,b,c) = f a b c

type instance FoldSig (a,b,c,d) x = (a -> b -> c -> d -> x)
instance Fold (a,b,c,d) where
        fold f (a,b,c,d) = f a b c d

-- some other standard algebraic types
type instance FoldSig (Maybe a) x = (x, a -> x)
instance Fold (Maybe a) where
        fold = fold maybe

type instance FoldSig (Either a b) x = (a -> x, b -> x)
instance Fold (Either a b) where
        fold = fold either

type instance FoldSig Bool x = (x, x)
instance Fold Bool where
        fold (a,b) x = if x then a else b

-- I imagine there could possibly be legitimate uses for something like this:
-- > type instance Enum a => FoldSig a x = Int -> x
-- > instance Enum a => Fold a where
-- >         fold = (. fromEnum)

