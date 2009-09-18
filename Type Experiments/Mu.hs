{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts, UndecidableInstances,
        TypeSynonymInstances,
        StandaloneDeriving,
        ViewPatterns,
        NoMonomorphismRestriction
  #-}
module Mu where

import Control.Monad

newtype Mu t = Mu (t (Mu t))
deriving instance Show (t (Mu t)) => Show (Mu t)
deriving instance Eq   (t (Mu t)) => Eq   (Mu t)
deriving instance Ord  (t (Mu t)) => Ord  (Mu t)

unMu (Mu t) = t
foldMu f (Mu x) = f (fmap (foldMu f) x)
mapMu f (Mu x) = Mu (f (fmap (mapMu f) x))

data Either1 f g x
    = Left1  (f x)
    | Right1 (g x)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Either1 f g) where
    fmap f (Left1  x) = Left1  (fmap f x)
    fmap f (Right1 x) = Right1 (fmap f x)

either1 f g (Left1  x) = f x
either1 f g (Right1 x) = g x

leftMu  x = Mu (Left1  x)
rightMu x = Mu (Right1 x)

eitherMu :: (f (Mu (Either1 f g)) -> h (Mu h))
         -> (g (Mu (Either1 f g)) -> h (Mu h))
         -> Mu (Either1 f g) -> Mu h
eitherMu f g (Mu (Left1  x)) = Mu (f x)
eitherMu f g (Mu (Right1 x)) = Mu (g x)


muJoin :: (Monad m, Functor m) => Mu m -> m a
muJoin = foldMu join

foldEitherMu
  :: (Functor f, Functor g) =>
     (f b -> b) -> (g b -> b) -> Mu (Either1 f g) -> b
foldEitherMu f g = foldMu (either1 f g)
