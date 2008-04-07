{-
 -      ``Control/Concurrent/STM/Worker.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Control.Concurrent.STM.Worker where

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

type AsyncRq m rq resp = rq -> m (TMVar resp)
type  SyncRq m rq resp = rq -> m resp

startSimpleSerialWorkerLoop :: (SyncRq IO rq resp) -> IO (AsyncRq STM rq resp)
startSimpleSerialWorkerLoop func = do
        let worker rqChan = forever $ do
                (rq, respVar) <- atomically $ readTChan rqChan
                resp <- func rq
                atomically $ putTMVar respVar resp
        
        startWorker worker

startWorker :: (TChan (rq, TMVar resp) -> IO ()) -> IO (AsyncRq STM rq resp)
startWorker worker = do
        rqChan <- newTChanIO
        
        forkIO (worker rqChan)
        
        let rqFunc rq = do
                respVar <- newEmptyTMVar
                writeTChan rqChan (rq, respVar)
                
                return respVar
                
        return rqFunc

toSyncRq :: AsyncRq STM rq resp -> SyncRq STM rq resp
toSyncRq asyncRq rq = do
        respVar <- asyncRq rq
        takeTMVar respVar