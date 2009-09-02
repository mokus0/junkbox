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

newtype MuT t = Mu (t (MuT t))
deriving instance Show (t (MuT t)) => Show (MuT t)
deriving instance Eq   (t (MuT t)) => Eq   (MuT t)
deriving instance Ord  (t (MuT t)) => Ord  (MuT t)

type Nat = MuT NatS
data NatS nat
    = Zero
    | Succ nat
    deriving (Eq, Ord, Show)

instance Enum Nat where
    toEnum 0     = mu Zero
    toEnum (n+1) = mu (Succ (toEnum n))
    fromEnum = foldMu fromE
        where
            fromE Zero = 0
            fromE (Succ s) = succ $! s

instance Functor NatS where
    fmap f Zero = Zero
    fmap f (Succ x) = Succ (f x)
zero = mu Zero
suc s = mu (Succ s)
inf = suc inf

instance (Mu mu, Show (mu NatS), Eq (mu NatS)) => Num (mu NatS) where
    x + y = foldMu addX y
        where
            addX Zero     = x
            addX (Succ y) = suc y
    
    x                   - (unMu -> Zero)        = x
    (unMu -> Zero)      - y                     = error "Nat.(-): negative result"
    (unMu -> (Succ x))  - (unMu -> (Succ y))    = (x - y)
    
    x * y = foldMu mulX y
        where
            mulX Zero     = zero
            mulX (Succ y) = x + y
    
    abs                     = id
    signum x@(unMu -> Zero) = x
    signum x                = 1
    
    fromInteger 0           = zero
    fromInteger (n+1)       = suc (fromInteger n)

data Either1 f g x
    = Left1  (f x)
    | Right1 (g x)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Either1 f g) where
    fmap f (Left1  x) = Left1  (fmap f x)
    fmap f (Right1 x) = Right1 (fmap f x)

either1 f g (Left1  x) = f x
either1 f g (Right1 x) = g x

leftMu  x = mu (Left1  x)
rightMu x = mu (Right1 x)

eitherMu f g (unMu -> Left1  x) = mu (f x)
eitherMu f g (unMu -> Right1 x) = mu (g x)

class Mu mu where
    -- simple injection
    mu     :: Functor t => t (mu t) -> mu t
    
    -- simple extraction
    unMu   :: Functor t => mu t -> t (mu t)
    unMu = foldMu (fmap mu)
    
    -- structure-ignoring reduction of the fixpointed functor
    foldMu :: (Functor t) => (t b -> b) -> mu t -> b
    foldMu f x = f (fmap (foldMu f) (unMu x))
    
    -- structure-preserving substitution of the fixpointed functor
    -- (default implementation drops structure)
    muMap :: (Functor f, Functor g) => (f (mu g) -> g (mu g)) -> mu f -> mu g
    muMap f x = mu (f (fmap (muMap f) (unMu x)))

instance Mu MuT where
    mu = Mu
    unMu (Mu x) = x
    foldMu f (Mu x) = f (fmap (foldMu f) x)
    muMap f (Mu x) = Mu (f (fmap (muMap f) x))


muJoin :: (Monad m, Functor m, Mu mu) => mu m -> m a
muJoin = foldMu join

foldEitherMu
  :: (Mu mu, Functor f, Functor g) =>
     (f b -> b) -> (g b -> b) -> mu (Either1 f g) -> b
foldEitherMu f g = foldMu (either1 f g)
