{-
 -      ``Util/STMIO.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Util.STMIO where

import Util.Misc

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad

import IO

type STMGet a = STM a
type STMPut a = a -> STM ()

type STMFlush = IO ()

-- TODO: dup() the handle?

stdStm :: IO (STMGet String, STMPut String, STMPut String, STMFlush)
stdStm = do
        stdin            <- stmInLines stdin
        (stdout, flush1) <- stmOut stdout
        (stderr, flush2) <- stmOut stderr
        
        return (stdin, stdout, stderr, flush1 >> flush2)

stmOut :: Handle -> IO (STMPut String, STMFlush)
stmOut handle = do
        hSetBuffering handle LineBuffering
        
        pending <- newTVarIO False
        chan <- newTChanIO :: IO (TChan String)
        
        -- STM Bool value indicating whether all pending output
        -- has been written to the stream
        let isFlushed = do
                pending <- readTVar pending
                if pending
                        then return False
                        else isEmptyTChan chan
        
        -- the writer thread
        forkIO $ forever $ do
                str <- atomically $ do
                        writeTVar pending True
                        readTChan chan
                hPutStr handle str
                atomically (writeTVar pending False)
        
        -- the interface to be returned
        let tPutStr = writeTChan chan
        let flush = do
                atomically (waitFor isFlushed)
                hFlush handle
        
        return (tPutStr, flush)

stmInUnbuf :: Handle -> IO (STMGet Char)
stmInUnbuf handle = do
        hSetBuffering handle NoBuffering
        
        chan <- newTChanIO :: IO (TChan Char)
        forkIO $ forever $ do
                char <- hGetChar handle
                atomically $ writeTChan chan char
        
        return (readTChan chan)

stmInLines :: Handle -> IO (STMGet String)
stmInLines handle = do
        hSetBuffering handle LineBuffering
        
        chan <- newTChanIO :: IO (TChan String)
        forkIO $ forever $ do
                str <- hGetLine handle
                atomically $ writeTChan chan str
        
        return (readTChan chan)
