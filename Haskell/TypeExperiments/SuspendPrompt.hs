{-# LANGUAGE RankNTypes, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module TypeExperiments.SuspendPrompt where

import Control.Monad.Prompt

-- The thought occurs to me that perhaps a further decomposition of the 
-- iteratee idea could lead to a more elegant implementation.  My first 
-- thought is that an iteratee is essentially like a defunctional "Prompt"
-- monad with one prompt operation, "get symbol".
--
-- Here is a sketch of a defunctionalized (i.e., non-CPS version of) 'Prompt'.
-- I wonder: can it be generalized to an operation that is blocking on 
-- _several_ prompts?

data Suspended p a where
    Done :: a -> Suspended p a
    Suspended :: p t -> (t -> Suspended p a) -> Suspended p a

instance Functor (Suspended p) where
    fmap f (Done x) = Done (f x)
    fmap f (Suspended p k) = Suspended p (fmap f . k)

instance Monad (Suspended p) where
    return = Done
    Done x >>= f = f x
    Suspended p k >>= f 
        = Suspended p (\x -> k x >>= f)

-- Suspended is isomorphic to prompt:
-- (other side of isomorphism is @runSuspendedM prompt@)
instance MonadPrompt p (Suspended p) where
    prompt p = Suspended p Done

suspend :: Prompt p a -> Suspended p a
suspend = runPromptM prompt

-- This type is equivalent to:
--      blockedOn :: Suspended p a -> (exists t. Maybe (p t))
blockedOn :: Suspended p a -> (forall t. Maybe (p t) -> x) -> x
blockedOn Done{} k = k Nothing
blockedOn (Suspended p _) k = k (Just p)

runSuspendedM ::  Monad m => (forall t. p t -> m t) -> Suspended p a -> m a
runSuspendedM f x = either (>>= runSuspendedM f) return (stepSuspendedM f x)

stepSuspendedM :: Monad m => (forall t. p t -> m t) -> Suspended p a -> Either (m (Suspended p a)) a
stepSuspendedM f (Done a) = Right a
stepSuspendedM f (Suspended p k) = Left $ do
    t <- f p
    return (k t)

-- runSuspendedM ::  Monad m => (forall t. p t -> m t) -> Suspended p a -> m a
-- runSuspendedM f x = stepSuspendedM f x >>= either (runSuspendedM f) return
-- 
-- stepSuspendedM :: Monad m => (forall t. p t -> m t) -> Suspended p a -> m (Either (Suspended p a) a)
-- stepSuspendedM f (Done a) = return (Right a)
-- stepSuspendedM f (Suspended p k) = do
--     t <- f p
--     return (Left (k t))
-- 

data GetSym a b where GetSym :: GetSym a (StreamG a)

type Iteratee s a = Suspended (GetSym s) a

-- non-monadic iteratee type from John Lato's Monad Reader 16 article:
data StreamG el = Empty | El el | EOF
data IterV el a 
    = DoneV a (StreamG el)
    | Cont (StreamG el -> IterV el a)

-- Isomorphism between @IterV el a@ and @Suspended (GetSym el) a@
f :: IterV el a -> Iteratee el (a, StreamG el)
f (DoneV x rest) = Done (x, rest)
f (Cont k) = Suspended GetSym (f . k)

g :: Iteratee el (a, StreamG el) -> IterV el a
g (Done (x,rest)) = DoneV x rest
g (Suspended GetSym k) = Cont (g . k)

-- Proof:
-- 
--      f.g = id
--      --------
-- 
-- f . g $ Done (x,rest)
-- f $ DoneV x rest
-- Done (x,rest)
-- 
-- f . g $ Suspended GetSym k
-- f $ Cont (g . k)
-- Suspended GetSym (f . g . k)
-- {-by induction-} Suspended GetSym k
-- 
--      g.f = id
--      --------
-- 
-- g . f $ DoneV x rest
-- g $ Done (x, rest)
-- DoneV x rest
-- 
-- g . f $ Cont k
-- g $ Suspended GetSym (f . k)
-- Cont (g . f . k)
-- {-by induction-} Cont k
