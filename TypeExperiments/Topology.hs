{-
 -      ``Topology''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances
  #-}

module TypeExperiments.Topology where

class SubSet s e | s -> e where
    contains :: s -> e -> Bool  -- parameterize over classifier?
    empty :: s
    full  :: s
    complement :: s -> s

class (Eq s, SubSet s e) => DecidableSubSet s e | s -> e where
    isEmpty :: s -> Bool
    isFull :: s -> Bool

instance SubSet (e -> Bool) e where
    contains = ($)
    empty = const False
    full = const True
    complement = (not .)

class SubSet s e => Topology t s e | s -> e where
    intersection :: t -> s -> s -> s
    union :: t -> s -> s -> s

data Computable = Computable

instance Topology Computable (e -> Bool) e where
    intersection Computable f g x = f x && g x
    union Computable f g x = f x || g x
