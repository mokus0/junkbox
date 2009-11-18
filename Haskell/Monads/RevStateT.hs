#!runhaskell
{-# OPTIONS -fglasgow-exts #-}
{-
 -	"RevState.hs"
 -	(c) 2007 James Cook
 -}

module RevState where

import Control.Monad.State

newtype RevStateT s m a = RevStateT {runRevStateT :: s -> m (a, s)}

y :: (a -> a) -> a
y f = f (y f)

{- in ghci, i have found an inhabitant of (>>=)'s type...  now, does it do anything interesting? 
	it seems to be very good at making infinite loops

x >>= f = RevStateT $ \s -> do (z, (s',s'')) <- y (composed x f s); return (z,s''))
	where
		composed x f s foo = do 
			(b, (s', s'')) <- foo
			(a, s'') <- runRevStateT x s'
			(b, s') <- runRevStateT (f a) s
			return (b, (s', s''))

-}

instance (Monad m) => Monad (RevStateT s m) where
	return a = RevStateT (\s -> return (a, s))
	fail err = error err
	x >>= f = RevStateT $ \s -> do 
		(z, (s',s'')) <- y (composed x f s)
		return (z,s'')
		where
			composed x f s foo = do 
				(b, (s', s'')) <- foo
				(a, s'') <- runRevStateT x s'
				(b, s') <- runRevStateT (f a) s
				return (b, (s', s''))


{-instance (Monad m) => Monad (RevStateT s m) where
    return a = RevStateT (\s -> return (a, s))
    fail err = error err
    x >>= f = RevStateT composed
	where
	    composed (s, s') = do 
		-- argh!  As i suspected... this is probably impossible...
		(a, s'') <- runRevStateT x s'
		(b, s') <- runRevStateT (f a) s
		return (b, (s', s''))

-}

instance (Monad m) => MonadState s (RevStateT s m)
    where
	get = RevStateT (\s -> return (s, s))
	put = \a -> RevStateT (\s -> return ((), a))

instance MonadTrans (RevStateT s)
	where
		lift x = RevStateT $ \s -> do
			x' <- x
			return (x', s)

{-
-- locally reverse state propagation, to allow for state updates in RevState
class (Monad m, Monad m') => MonadReversible m m' | m' -> m
    where
	reverseMonad :: m a -> m' a

instance MonadReversible (RevStateT s m) (StateT s m)
    where
	reverseMonad (RevStateT f) = StateT f

instance MonadReversible (StateT s m) (RevStateT s m)
    where
	reverseMonad (StateT f) = RevStateT f

-}