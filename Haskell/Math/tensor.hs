{-# LANGUAGE GADTs, KindSignatures, TypeFamilies #-}
module Math.Tensor where

newtype Identity t = Identity t
newtype Compose f g t = Compose (f (g t))

class Container c where
    type Index c
    index :: c e -> Index c -> e

instance Container [] where
    type Index [] = Int
    index = (!!)

data Tensor sig where
    Elem :: t -> Tensor t
    Vect :: Container f => f (Tensor t) -> Tensor (Index f -> t)

tensor :: Tensor sig -> sig
tensor (Elem e) = e
tensor (Vect v) = \i -> tensor (v `index` i)