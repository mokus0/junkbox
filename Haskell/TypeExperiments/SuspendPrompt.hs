{-# LANGUAGE RankNTypes, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module TypeExperiments.SuspendPrompt where

import Control.Monad.Prompt

-- runPromptM :: (Monad m) => (forall a. p a -> m a) -> Prompt p r -> m r

-- |Essentially a defunctionalized (i.e., non-CPS version of) 'Prompt'.
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
