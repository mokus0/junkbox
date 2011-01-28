{-# LANGUAGE
        TypeFamilies
  #-}
module TypeExperiments.Concurrent where

import Control.Concurrent
import Control.Exception
import Control.Monad.Identity
import Control.Parallel

class Concurrent f where
    data Future f :: * -> *
    future :: f a -> f (Future f a)
    
    check :: Future f a -> f (Maybe a)
    wait  :: Future f a -> f a

instance Concurrent IO where
    -- outer mvar is for synchronization.  Without it, a race condition would 
    -- exist within 'check' which would cause it to potentially return 
    -- 'Nothing' if called concurrently from 2 different threads.
    newtype Future IO a = IOFuture (MVar (MVar (Either SomeException a)))
    future x = do
        f <- newMVar =<< newEmptyMVar
        forkIO (withMVar f . flip putMVar =<< try x)
        return (IOFuture f)
    
    check (IOFuture f) = withMVar f $ \inner -> do
        r <- tryTakeMVar inner
        case r of
            Nothing -> return Nothing
            Just x  -> do
                putMVar inner x
                either throwIO (return . Just) x
    
    wait (IOFuture f) = do
        inner <- readMVar f
        x <- readMVar inner
        either throwIO return x

instance Concurrent Identity where
    data Future Identity x = LazyFuture x
    future (Identity x) = x `par` Identity (LazyFuture x)
    
    check (LazyFuture x) = Identity (Just x)
    wait  (LazyFuture x) = x `seq` Identity x

-- class Concurrent f => Select f where
--     merge  :: Future f a -> Future f b -> f (Future f (Either a b))
--     select :: [Future f a] -> f a
    
