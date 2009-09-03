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
unMuT (Mu t) = t
foldMuT f (Mu x) = f (fmap (foldMuT f) x)
deriving instance Show (t (MuT t)) => Show (MuT t)
deriving instance Eq   (t (MuT t)) => Eq   (MuT t)
deriving instance Ord  (t (MuT t)) => Ord  (MuT t)

type Nat = MuT NatS
data NatS nat
    = Zero
    | Succ nat
    deriving (Eq, Ord, Show)

instance Enum Nat where
    toEnum 0     = Mu Zero
    toEnum (n+1) = Mu (Succ (toEnum n))
    fromEnum = foldMuT fromE
        where
            fromE Zero = 0
            fromE (Succ s) = succ $! s

instance Functor NatS where
    fmap f Zero = Zero
    fmap f (Succ x) = Succ (f x)
zero = mu Zero
suc s = mu (Succ s)
inf = suc inf

instance Num (MuT NatS) where
    x + y = foldMuT addX y
        where
            addX Zero     = x
            addX (Succ y) = suc y
    
    x                   - (unMuT -> Zero)       = x
    (unMuT -> Zero)     - y                     = error "Nat.(-): negative result"
    (unMuT -> (Succ x)) - (unMuT -> (Succ y))   = (x - y)
    
    x * y = foldMuT mulX y
        where
            mulX Zero     = zero
            mulX (Succ y) = x + y
    
    abs                         = id
    signum x@(unMuT -> Zero)    = x
    signum x                    = 1
    
    fromInteger 0               = zero
    fromInteger (n+1)           = suc (fromInteger n)

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

eitherMu f g (unMuT -> Left1  x) = mu (f x)
eitherMu f g (unMuT -> Right1 x) = mu (g x)

class Mu mu where
    -- simple injection
    mu     :: Functor t => t (mu t) -> mu t
    
    -- simple extraction
    unMu   :: Functor t => mu t -> [t (mu t)]
    -- generic instance which destroys inner structure:
    -- unMu = foldMu (fmap mu)
    
    -- structure-ignoring reduction of the fixpointed functor
    foldMu :: (Functor t) => ([t b] -> b) -> mu t -> b
    foldMu f x = f (map (fmap (foldMu f)) (unMu x))
    
    -- structure-preserving substitution of the fixpointed functor
    mapMu :: (Functor f, Functor g) => (f (mu g) -> g (mu g)) -> mu f -> mu g
    -- a simple generic implementation which drops any extra structure:
    -- mapMu f x = mu (f (fmap (mapMu f) (unMu x)))

instance Mu MuT where
    mu = Mu
    unMu (Mu x) = [x]
    foldMu f (Mu x) = f [fmap (foldMu f) x]
    mapMu f (Mu x) = Mu (f (fmap (mapMu f) x))



--muJoin :: (Monad m, Functor m, Mu mu) => mu m -> m a
--muJoin = foldMu join

--foldEitherMu
--  :: (Mu mu, Functor f, Functor g) =>
--     (f b -> b) -> (g b -> b) -> mu (Either1 f g) -> b
--foldEitherMu f g = foldMu (either1 f g)
