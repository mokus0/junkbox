{-# LANGUAGE 
    MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances
  #-}
{-
 -      ``StateRef.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Data.StateRef (
        module Data.StateRef,
        
        IORef, STRef, TVar
        ) where

import Control.Monad
import Control.Arrow

import Data.IORef

import Control.Monad.ST
import Data.STRef

import Control.Concurrent.STM

class (StateRef sr m a) => NewStateRef sr m a where
        newRef :: a -> m sr

class (Monad m) => StateRef sr m a | sr -> a where
        readRef :: sr -> m a
        writeRef :: sr -> a -> m ()
        
        modifyRef :: sr -> (a -> a) -> m (a, a)
        modifyRef ref f = do
                x <- readRef ref
                let x' = f x
                writeRef ref x'
                return (x, x')

class (NewStateRef sr m a,
       StateRef sr m a)
       => DefaultStateRef sr m a | m a -> sr where
        newRef' :: a -> m sr
        newRef' = newRef
        readRef' :: sr -> m a
        readRef' = readRef
        writeRef' :: sr -> a -> m ()
        writeRef' = writeRef
        modifyRef' :: sr -> (a -> a) -> m (a, a)
        modifyRef' = modifyRef

instance DefaultStateRef (IORef a) IO a
instance NewStateRef (IORef a) IO a where
        newRef = newIORef
instance StateRef (IORef a) IO a where
        readRef = readIORef
        writeRef = writeIORef
        modifyRef ref f = atomicModifyIORef ref (f &&& id &&& f)

instance DefaultStateRef (STRef s a) (ST s) a
instance NewStateRef (STRef s a) (ST s) a where
        newRef = newSTRef
instance StateRef (STRef s a) (ST s) a where
        readRef = readSTRef
        writeRef = writeSTRef

instance NewStateRef (STRef RealWorld a) IO a where
        newRef = stToIO . newRef
instance StateRef (STRef RealWorld a) IO a where
        readRef = stToIO . readRef
        writeRef r = stToIO . writeRef r
        modifyRef r = stToIO . modifyRef r

instance DefaultStateRef (TVar a) STM a
instance NewStateRef (TVar a) STM a where
        newRef = newTVar
instance StateRef (TVar a) STM a where
        readRef = readTVar
        writeRef = writeTVar

instance NewStateRef (TVar a) IO a where
        newRef                  = newTVarIO
instance StateRef (TVar a) IO a where
        readRef ref             = atomically (readTVar ref)
        writeRef ref val        = atomically (writeTVar ref val)
        modifyRef ref f         = atomically (modifyRef ref f)

-- this is an instance I would like to make, but it opens
-- a big can of worms... it requires incoherent instances, for one.
-- perhaps I ought to give up the abstractness of 'sr' in the class
-- definition; i don't know if that gets me anywhere though... 
--
-- note that as long as only these instances exist, there is no
-- actual overlap.  maybe it's not such a bad idea.  on the other
-- hand, a corresponding instance for Reader would be nice too, and
-- that one does have potential overlap.
--
-- instance (MonadState s1 m,
--           StateRef s2 m a)
--                 => StateRef (s1 -> s2) m a
--         where
--                 readRef f       = do
--                         s1 <- get
--                         readRef (f s1)
--                 writeRef f val  = do
--                         s1 <- get
--                         writeRef (f s1) val
--                 modifyRef f g = do
--                         s1 <- get
--                         modifyRef (f s1) g

-- can't use fmap, because Functor isn't a superclass of Monad, 
-- despite the fact that every Monad is a functor.
readsRef :: (StateRef sr m a,
             Monad m) =>
            sr -> (a -> b) -> m b
readsRef r f = do
        x <- readRef r
        return (f x)

newCounter :: (DefaultStateRef sr m1 a,
               NewStateRef sr m a,
               Enum a) =>
              a -> m (m1 a)
newCounter n = do
        c <- newRef n
        return $ do
                (x, x') <- modifyRef' c succ
                return x
