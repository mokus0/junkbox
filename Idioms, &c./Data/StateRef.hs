{-# LANGUAGE 
    MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances
  #-}
{-
 -      ``StateRef.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Data.StateRef where

import Control.Monad
import Control.Arrow

import Data.IORef

import Control.Monad.ST
import Data.STRef

import Control.Concurrent.STM

class (Monad m) => StateRef sr m a | sr -> a where
        newRef :: a -> m sr
        readRef :: sr -> m a
        writeRef :: sr -> a -> m ()
        
        modifyRef :: sr -> (a -> a) -> m (a, a)
        modifyRef ref f = do
                x <- readRef ref
                let x' = f x
                writeRef ref x'
                return (x, x')

class (StateRef sr m a) => DefaultStateRef sr m a | sr -> m a, m a -> sr where
        newRef' :: a -> m sr
        newRef' = newRef

instance DefaultStateRef (IORef a) IO a
instance StateRef (IORef a) IO a where
        newRef = newIORef
        readRef = readIORef
        writeRef = writeIORef
        modifyRef ref f = atomicModifyIORef ref (f &&& id &&& f)

instance DefaultStateRef (STRef s a) (ST s) a
instance StateRef (STRef s a) (ST s) a where
        newRef = newSTRef
        readRef = readSTRef
        writeRef = writeSTRef

instance DefaultStateRef (TVar a) STM a
instance StateRef (TVar a) STM a where
        newRef = newTVar
        readRef = readTVar
        writeRef = writeTVar

instance StateRef (TVar a) IO a where
        newRef                  = newTVarIO
        readRef ref             = atomically (readTVar ref)
        writeRef ref val        = atomically (writeTVar ref val)
        modifyRef ref f         = atomically (modifyRef ref f)

-- > refMaybe ||= computation
-- >         = do
-- >                 old <- readRef refMaybe
-- >                 case old of
-- >                         Nothing -> do
-- >                                 new <- computation
-- >                                 writeRef refMaybe new
-- >                         Just _  -> return ()

readsRef r f = do
        x <- readRef r
        return (f x)

newCounter n = do
        c <- newRef' n
        return $ do
                x <- readRef c
                writeRef c (succ x)
                return x

ref ||= computation
        = do
                old <- readRef ref
                if old == mzero
                        then do
                                new <- computation
                                writeRef ref new
                        else return ()
