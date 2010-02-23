{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module TypeExperiments.Prompt where

import Control.Monad.Prompt
import Control.Monad.State
import Control.Monad.Free

data StatePrompt s a where
    Get :: StatePrompt s s
    Put :: s -> StatePrompt s ()

instance MonadState s (Prompt (StatePrompt s)) where
    get = prompt Get
    put x = prompt (Put x)

reifyState :: MonadState s m => StatePrompt s a -> m a
reifyState Get      = get
reifyState (Put x)  = put x

immutableState :: s -> StatePrompt s a -> a
immutableState s Get = s
immutableState _ (Put _) = ()

reportingImmutableState :: Show s => s -> StatePrompt s a -> IO a
reportingImmutableState s Get = return s
reportingImmutableState _ (Put s) = putStrLn ("Tried to set state to: " ++ show s ++ " (permission denied!  mwa ha ha!)")

