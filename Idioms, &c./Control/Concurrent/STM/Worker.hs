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

startSimpleSerialWorkerLoop :: (SyncRq IO rq resp) 
                            -> IO (AsyncRq STM rq resp)
startSimpleSerialWorkerLoop func = do
        let worker rqChan = forever $ do
                (rq, respVar) <- atomically $ readTChan rqChan
                resp <- func rq
                atomically $ putTMVar respVar resp
        
        startWorker worker

makeRqFunc :: (rq -> TMVar resp -> wrapper)
           -> TChan wrapper
           -> AsyncRq STM rq resp
makeRqFunc wrapper rqChan = \rq -> do
        respVar <- newEmptyTMVar
        writeTChan rqChan (wrapper rq respVar)
        
        return (takeTMVar respVar)

makeStdRqChan :: IO ( AsyncRq STM rq resp
                    , TChan (rq, TMVar resp)
                    )
makeStdRqChan = do
        rqChan <- newTChanIO
        return (makeRqFunc (,) rqChan, rqChan)

startWorker :: (TChan (rq, TMVar resp) -> IO ())
            -> IO (AsyncRq STM rq resp)
startWorker worker = do
        (rqFunc, rqChan) <- makeStdRqChan
        
        forkIO (worker rqChan)
                
        return rqFunc

-- Think of rq as a GADT that defines a request interface.
makePolymorphicRqChan :: (forall resp. rq resp -> TMVar resp -> wrapper)
                      -> IO ( forall resp. AsyncRq STM (rq resp) resp
                             , TChan wrapper
                             )
makePolymorphicRqChan wrapper = do
        rqChan <- newTChanIO
        return (makeRqFunc wrapper rqChan, rqChan)

startPolymorphicWorker :: (forall resp. rq resp -> TMVar resp -> wrapper)
                       -> (TChan wrapper -> IO ())
                       -> IO (forall resp. AsyncRq STM (rq resp) resp)
startPolymorphicWorker wrapper worker = do
        (rqFunc, rqChan) <- makePolymorphicRqChan wrapper
        
        forkIO (worker rqChan)
        
        return rqFunc
