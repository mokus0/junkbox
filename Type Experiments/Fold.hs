{-
 -      ``Fold''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeFamilies,
        RankNTypes
  #-}

module Fold where
        
import Data.List

type family FoldSig t a :: *
class Fold t where
        fold :: forall a. FoldSig t a -> t -> a

type family UnfoldSig t a :: *
class Unfold t where
        unfold :: forall a. UnfoldSig t a -> a -> t

type instance FoldSig [x] a = (a, x -> a -> a)
instance Fold [x] where
        fold (nil, cons) = foldr cons nil

type instance UnfoldSig [x] a = a -> Either () (x, a)
instance Unfold [x] where
        unfold f = unfoldr (unfold f)

-- some small tuples...
 -- the 'fold' operation is only slightly nontrivial,
 -- only first-class functions allow it to be structurally nontrivial,
 -- and only laziness allows it to be operationally nontrivial.
 -- 'folding' in this sense is 'currying'.
type instance FoldSig (a, b) x = (a -> b -> x)
instance Fold (a,b) where
        fold = uncurry

type instance UnfoldSig (a,b) x = (x -> a, x -> b)
instance Unfold (a,b) where
        unfold (f, g) x = (f x, g x)

type instance FoldSig (a, b, c) x = (a -> b -> c -> x)
instance Fold (a,b,c) where
        fold f (a,b,c) = f a b c

type instance UnfoldSig (a,b,c) x = (x -> a, x -> b, x -> c)
instance Unfold (a,b,c) where
        unfold (f, g, h) x = (f x, g x, h x)

type instance FoldSig (a,b,c,d) x = (a -> b -> c -> d -> x)
instance Fold (a,b,c,d) where
        fold f (a,b,c,d) = f a b c d

type instance UnfoldSig (a,b,c,d) x = (x -> a, x -> b, x -> c, x -> d)
instance Unfold (a,b,c,d) where
        unfold (f, g, h, i) x = (f x, g x, h x, i x)

-- some other standard algebraic types
-- Maybe:
type instance FoldSig (Maybe a) x = (x, a -> x)
instance Fold (Maybe a) where
        fold = fold maybe

type instance UnfoldSig (Maybe a) x = x -> Either () a
instance Unfold (Maybe a) where
        unfold f x = either (const Nothing) Just (f x)

-- Either:
type instance FoldSig (Either a b) x = (a -> x, b -> x)
instance Fold (Either a b) where
        fold = fold either

  -- Despite the dramatically different structure of the type, this
  -- is as trivial as the |FoldSig (a,b) x| instance, and similarly relies
  -- on first class functions and on partial functions.
type instance UnfoldSig (Either a b) x = (x -> Bool, x -> a, x -> b)
instance Unfold (Either a b) where
        unfold (p, f, g) x = if p x then Left (f x) else Right (g x)

-- Bool:
type instance FoldSig Bool x = (x, x)
instance Fold Bool where
        fold (a,b) x = if x then a else b

-- I see no sensible way to make this nontrivial
type instance UnfoldSig Bool x = x -> Bool
instance Unfold Bool where
        unfold = id

-- (->):
type instance FoldSig (a -> b) x = (a, b -> x)
instance Fold (a -> b) where
        fold (a, g) f = g (f a)

  -- I'm not sure this makes a lot of sense.  It does seem intuitively to
  -- be related to the FoldSig instances for tuples, but it doesn't seem 'natural'.
type instance UnfoldSig (a -> b) x = (x, a) -> b
instance Unfold (a -> b) where
        unfold = curry

-- I imagine there could possibly be legitimate uses for something like this:
-- > type instance Enum a => FoldSig a x = Int -> x
-- > instance Enum a => Fold a where
-- >         fold = (. fromEnum)
