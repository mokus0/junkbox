{-
 -      ``Control/Monad/Loops''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Control.Monad.Loops where

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

atomLoop :: STM a -> IO ()
atomLoop = forever . atomically

forkLoop :: STM a -> IO ThreadId
forkLoop = forkIO . atomLoop

{-# SPECIALIZE while :: IO Bool -> IO a -> IO () #-}

while :: (Monad m) => m Bool -> m a -> m ()
while p f = do
        x <- p
        if x
                then do
                        f
                        while p f
                else return ()

{-# SPECIALIZE whileJust :: IO (Maybe a) -> (a -> IO b) -> IO () #-}

whileJust :: (Monad m) => m (Maybe a) -> (a -> m b) -> m ()
whileJust p f = do
        x <- p
        case x of
                Nothing -> return ()
                Just x  -> do
                        f x
                        whileJust p f

{-# SPECIALIZE unfoldM :: (Monad m) => m (Maybe a) -> m [a] #-}
{-# SPECIALIZE unfoldM :: IO (Maybe a) -> IO [a] #-}

unfoldM :: (Monad m, MonadPlus f) => m (Maybe a) -> m (f a)
unfoldM m = do
        x <- m
        case x of
                Nothing -> return mzero
                Just x  -> do
                        xs <- unfoldM m
                        return (return x `mplus` xs)
