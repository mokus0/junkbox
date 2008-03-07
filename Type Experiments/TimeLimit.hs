{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies #-}
{-
 -	"TimeLimit.hs"
 -	(c) 2008 James Cook
 -}

module TimeLimit where

class (Monad m) => MonadTimed s m | m -> s where
  tentative :: s -> m ()
  bestSolution :: m s

newtype TimedT s m a = TimedT {runTimedT :: TVar s -> m a}

