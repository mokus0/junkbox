{-# LANGUAGE 
        RankNTypes,
        NoMonomorphismRestriction
  #-}  
{-
 -      ``Coprod.hs''
 -      (c) 2008 James Cook
 -}

module Coprod where

import Control.Monad
import Control.Monad.Identity

import Triple

data Plus t1 t2 a 
        = Var a
        | Inl (t1 (Plus t1 t2 a))
        | Inr (t2 (Plus t1 t2 a))
--        deriving (Eq, Show)

inl x = Inl (fmap return x)
inr x = Inr (fmap return x)

fold :: (Functor t1, Functor t2) =>
        (a -> b) -> (t1 b -> b) -> (t2 b -> b) -> Plus t1 t2 a -> b
fold e f1 f2 (Var a)    = e a
fold e f1 f2 (Inl a)    = f1 (fmap (fold e f1 f2) a)
fold e f1 f2 (Inr a)    = f2 (fmap (fold e f1 f2) a)

strip1 :: (Triple t1) => t1 (Plus t1 t2 a) -> Plus t1 t2 a
strip1 t = case etaInv t of
        Just x  -> x
        Nothing -> Inl t

strip2 :: (Triple t2) => t2 (Plus t1 t2 a) -> Plus t1 t2 a
strip2 t = case etaInv t of
        Just x  -> x
        Nothing -> Inr t

strip = fold Var strip1 strip2

lift1 (Inl t)   = t
lift1 t         = eta t

lift2 (Inr t)   = t
lift2 t         = eta t

wit1 t = strip1 (mu (fmap lift1 t))
wit2 t = strip2 (mu (fmap lift2 t))
wit = fold Var wit1 wit2

coprod :: (Functor t1, Functor t2, Monad s)
        => (forall a. t1 a -> s a)
        -> (forall a. t2 a -> s a)
        -> Plus t1 t2 a
        -> s a
coprod f g = fold return (join.f) (join.g)

coprod' :: (Functor t1, Functor t2)
        => (forall a. t1 a -> a)
        -> (forall a. t2 a -> a)
        -> Plus t1 t2 a
        -> a
coprod' f g x = r
        where
                Identity r = coprod (Identity . f) (Identity . g) x

in1 = inl
in2 = inr . inl
in3 = inr . inr . inl
in4 = inr . inr . inr . inl
in5 = inr . inr . inr . inr . inl

instance (Functor t1, Functor t2) => Functor (Plus t1 t2)
        where
                fmap f (Var a) = Var (f a)
                fmap f (Inl a) = Inl (fmap (fmap f) a)
                fmap f (Inr a) = Inr (fmap (fmap f) a)

instance (Functor t1, Triple t1, Functor t2, Triple t2) => Triple (Plus t1 t2)
        where
                -- eta :: a -> Plus t1 t2 a
                eta             = Var
                etaInv          = fold Just (join.etaInv) (join.etaInv)
                -- mu :: Plus t1 t2 (Plus t1 t2 a) -> Plus t1 t2 a
                mu = fold id wit1 wit2
