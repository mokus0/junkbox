{-# OPTIONS 
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XFlexibleInstances
  #-}
{-
 -      ``StateRef.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Data.StateRef where

import Data.Monoid
import Control.Monad

import Data.IORef
import Control.Concurrent.STM

class StateRef sr m a | sr -> m a where
        newRef :: a -> m sr
        readRef :: sr -> m a
        writeRef :: sr -> a -> m ()

instance StateRef (IORef a) IO a where
        newRef = newIORef
        readRef = readIORef
        writeRef = writeIORef

instance StateRef (TVar a) STM a where
        newRef = newTVar
        readRef = readTVar
        writeRef = writeTVar

-- > refMaybe ||= computation
-- >         = do
-- >                 old <- readRef refMaybe
-- >                 case old of
-- >                         Nothing -> do
-- >                                 new <- computation
-- >                                 writeRef refMaybe new
-- >                         Just _  -> return ()


ref ||= computation
        = do
                old <- readRef ref
                if old == mzero
                        then do
                                new <- computation
                                writeRef ref new
                        else return ()
