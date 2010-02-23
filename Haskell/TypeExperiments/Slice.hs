{-# LANGUAGE ExistentialQuantification, GADTs, RankNTypes #-}
module TypeExperiments.Slice where

newtype Slice a t b = Slice (a -> t b)
instance Functor t => Functor (Slice a t) where
    fmap f (Slice g) = Slice (fmap f . g)

mapSlice :: Functor t => (b -> b') -> Slice a t b -> Slice a t b'
mapSlice = fmap

comapSlice :: (a -> a') -> Slice a' t b -> Slice a t b
comapSlice f (Slice g) = Slice (g . f)

newtype Coslice a t b = Coslice (t a -> b)

instance Functor (Coslice a t) where
    fmap f (Coslice g) = Coslice (f . g)


mapCoslice :: (b -> b') -> Coslice a t b -> Coslice a t b'
mapCoslice = fmap

comapCoslice :: Functor t => (a -> a') -> Coslice a' t b -> Coslice a t b
comapCoslice f (Coslice g) = Coslice (g . fmap f)

-- universal arrow as initial object in slice category
data Universal c s = forall r. Universal (Slice c s r) (forall d. Slice c s d -> (r -> d))

-- an existentially-quantified pair containing the factorization of an arbitrary slice object
data Factorization f t = forall r. Factorization (f r) (r -> t)

factorThrough :: Slice c s d -> Universal c s -> Factorization (Slice c s)d
f `factorThrough` Universal u r = Factorization u (r f)