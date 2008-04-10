{-# LANGUAGE
        Rank2Types
  #-}
{-
 -      ``Control/Concurrent/STM/Worker.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Control.Concurrent.STM.Worker where

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

type AsyncRq m rq resp = rq -> m (m resp)
type  SyncRq m rq resp = rq -> m resp

startSimpleSerialWorkerLoop :: (SyncRq IO rq resp) -> IO (AsyncRq STM rq resp)
startSimpleSerialWorkerLoop func = do
        let worker rqChan = forever $ do
                (rq, respVar) <- atomically $ readTChan rqChan
                resp <- func rq
                atomically $ putTMVar respVar resp
        
        startWorker worker

-- Think of rq as a GADT, and this might make sense.  Unfortunately I know of
-- no way to type this so that it can share code with startWorker.
-- (disregarding types, | startWorker = startPolymorphicWorker (,) |).
-- I suppose I could use Template Haskell, and give the end user the power
-- to assign any type they want.
startPolymorphicWorker :: (forall resp. rq resp -> TMVar resp -> wrapper) 
                       -> (TChan wrapper -> IO ())
                       -> IO (forall resp. AsyncRq STM (rq resp) resp)
startPolymorphicWorker wrapper worker = do
        rqChan <- newTChanIO
        
        forkIO (worker rqChan)
        
        let rqFunc rq = do
                respVar <- newEmptyTMVar
                writeTChan rqChan (wrapper rq respVar)
                
                return (takeTMVar respVar)
                
        return rqFunc

startWorker :: (TChan (rq, TMVar resp) -> IO ()) -> IO (AsyncRq STM rq resp)
startWorker worker = do
        rqChan <- newTChanIO
        
        forkIO (worker rqChan)
        
        let rqFunc rq = do
                respVar <- newEmptyTMVar
                writeTChan rqChan (rq, respVar)
                
                return (takeTMVar respVar)
                
        return rqFunc

toSyncRq :: AsyncRq STM rq resp -> SyncRq IO rq resp
toSyncRq asyncRq rq = do
        resp <- atomically $ asyncRq rq
        atomically $ resp