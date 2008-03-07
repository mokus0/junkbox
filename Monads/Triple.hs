{-# OPTIONS 
        -fno-monomorphism-restriction
        -fglasgow-exts
        -fallow-overlapping-instances
        -fallow-undecidable-instances
        -fallow-incoherent-instances
  #-}
{-
 -      ``Triple.hs''
 -      (c) 2008 James Cook
 -}

module Triple where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import RevState

class (Functor f) => Triple f where
        eta     :: a -> f a
        etaInv  :: f a -> Maybe a
        mu      :: f (f a) -> f a

-- class (Monad m) => ReturnInv m where
--         returnInv :: m a -> Maybe a
-- 
-- instance ReturnInv Identity where
--         returnInv = Just . runIdentity
-- 
-- instance ReturnInv Maybe where
--         returnInv = id
-- 
-- instance ReturnInv ((->) a) where
--         returnInv f = Nothing
-- 
-- instance ReturnInv [] where
--         returnInv [x]   = Just x;
--         returnInv _     = Nothing
-- 
-- instance ReturnInv (State s) where
--         returnInv = const Nothing
-- 
-- instance ReturnInv (RevState s) where
--         returnInv = const Nothing
-- 
-- instance (Monad m) => ReturnInv (StateT s m) where
--         returnInv = const Nothing

instance (Triple t) => Monad t where
        x >>= f         = (mu . fmap f) x
        return = eta

--instance (Functor f, Monad f) => Triple f where
--        eta = return
--        etaInv = const Nothing
--        mu = join

--instance (Functor f, Monad f, ReturnInv f) => Triple f where
--        eta = return
--        etaInv = returnInv
--        mu x = x >>= id

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

instance Triple [] where
        eta x           = [x]
        etaInv [x]      = Just x
        etaInv _        = Nothing
        mu              = concat

instance Triple Identity where
        eta             = return
        etaInv          = Just . runIdentity
        mu              = join

instance Triple (Writer String) where
        eta             = return
        etaInv          = const Nothing
        mu              = join

instance (Functor t1, Functor t2) => Functor (Plus t1 t2)
        where
                fmap f (Var a) = Var (f a)
                fmap f (Inl a) = Inl (fmap (fmap f) a)
                fmap f (Inr a) = Inr (fmap (fmap f) a)

instance (Triple t1, Triple t2) => Triple (Plus t1 t2)
        where
                -- eta :: a -> Plus t1 t2 a
                eta             = Var
                etaInv          = fold Just (join.etaInv) (join.etaInv)
                -- mu :: Plus t1 t2 (Plus t1 t2 a) -> Plus t1 t2 a
                mu = fold id wit1 wit2
