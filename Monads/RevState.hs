#!runhaskell
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}
{-
 -	"RevState.hs"
 -	(c) 2007 James Cook
 -}

module RevState where

import Control.Monad.State
import Control.Arrow

newtype RevState s a = RevState {runRevState :: s -> (a, s)}

instance Functor (RevState s) where
        fmap f (RevState x) = RevState (fmap (f *** id) x)

instance Monad (RevState s) where
    return a = RevState (\s -> (a, s))
    fail err = error err
    x >>= f = RevState composed
	where
	    composed s = (b, s'')
		where
		    (a, s'') = runRevState x s'
		    (b, s') = runRevState (f a) s

instance MonadState s (RevState s)
    where
	get = RevState (\s -> (s, s))
	put = \a -> RevState (\s -> ((), a))

-- locally reverse state propagation, to allow for state updates in RevState
class (Monad m, Monad m') => MonadReversible m m' | m' -> m
    where
	reverseMonad :: m a -> m' a

instance MonadReversible (RevState s) (State s)
    where
	reverseMonad (RevState f) = State f

instance MonadReversible (State s) (RevState s)
    where
	reverseMonad (State f) = RevState f

-- simple example.  Same code, 2 ways of propagating state.
a :: (MonadState Integer m, MonadState Integer m', MonadReversible m' m) => m Integer
a = do
    x <- get
    
    reverseMonad $ do
	y <- get
	put (y ^ 2)
    
    return x

-- |runA == id|
runA x = fst $ runState a x

-- |runA' == (^2)|
runA' x = fst $ runRevState a x