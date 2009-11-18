{-# LANGUAGE
        Rank2Types, ExistentialQuantification, NoMonomorphismRestriction, NoMonoPatBinds
  #-}
{-
 -      ``Control/Concurrent/STM/Worker.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Control.Concurrent.STM.Worker where

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

-- It may be tempting, but DON'T join the result of an (AsyncRq STM)!
-- Use ToSyncRq instead.
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

toSyncRq :: AsyncRq STM rq resp
         -> SyncRq IO rq resp
toSyncRq async rq = do
        respVar <- atomically (async rq)
        atomically respVar

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

-- ghc-6.9.03132008's (,) doesn't play nice with the types I was using in, so
--   in the grand ghc tradition of calling things stupid when they don't fit into my worldview:
data StupidTuple rq wrapper = forall resp. StupidTuple (AsyncRq STM (rq resp) resp) (TChan wrapper)

-- Think of rq as a GADT that defines a request interface.
makePolymorphicRqChan :: (forall resp. rq resp -> TMVar resp -> wrapper)
                        -> IO (StupidTuple rq wrapper)
--                      -> IO ( forall resp. AsyncRq STM (rq resp) resp
--                             , TChan wrapper
--                             )
makePolymorphicRqChan wrapper = do
        rqChan <- newTChanIO
        return (StupidTuple (makeRqFunc wrapper rqChan) rqChan)

-- BROKEN on 6.9.03132008, and I can't seem to fix it.  Hopefully just a transient compiler glitch.
--startPolymorphicWorker :: (forall resp. rq resp -> TMVar resp -> wrapper)
--                       -> (TChan wrapper -> IO ())
--                       -> IO (forall resp. AsyncRq STM (rq resp) resp)
--startPolymorphicWorker wrapper worker = do
--        stupidTuple <- makePolymorphicRqChan wrapper
--        
--        case stupidTuple of
--                StupidTuple rqFunc rqChan -> do
--                        forkIO (worker rqChan)
--        
--                        return rqFunc
--