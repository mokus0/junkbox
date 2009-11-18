{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
{-
 -      ``/Users/mokus/Projects/mokus-sandbox/Monads/Expr3.hs''
 -      (c) 2008 James Cook
 -}

module Expr3 where

import Prelude hiding (pred, succ)
import Triple

data E a
        = Free a
        | Bound Int
        | App (E a) (E a)
        deriving (Eq, Show)

foldE f1 f2 f3 (Free v)         = f1 v
foldE f1 f2 f3 (Bound i)        = f2 i
foldE f1 f2 f3 (App x y)        = f3 (foldE f1 f2 f3 x) (foldE f1 f2 f3 y)

data L a
        = R a
        | L (L a)
        deriving (Eq, Show)

foldL f1 f2 (R x)               = f1 x
foldL f1 f2 (L x)               = f2 (foldL f1 f2 x)

type Expr = Plus E L

instance Functor E where
        fmap f (Free x)         = Free (f x)
        fmap f (Bound i)        = Bound i
        fmap f (App x y)        = App (fmap f x) (fmap f y)

instance Functor L where
        fmap f                  = foldL (R . f) L

instance Triple E where
        eta                     = Free
        
        etaInv (Free x)         = Just x
        etaInv _                = Nothing
        
        mu = foldE id Bound App

instance Triple L where
        eta                     = R
        
        etaInv (R x)            = Just x
        etaInv _                = Nothing
        
        mu                      = foldL id L

succ n = \f x -> (f (n f x))
pred n = \f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u)

instance (Num a) => Show ((a -> a) -> a -> a) where
        showsPrec p f = showsPrec p (f (+1) 0)