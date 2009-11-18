#!/usr/bin/env runhaskell
{-
 -      ``nexus''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Main where

import Control.Monad
import Control.Monad.Instances
import Data.List

import System.Environment

import Control.Concurrent
import Control.Concurrent.STM
import Data.StateRef
import Data.Channel
import Util.STMIO
import Control.Monad.Loops

import qualified Data.Map as M

import System.Posix.IO
import IO
import System.Process
import Control.Exception

mapTail f [] = []
mapTail f (x:xs) = x : (map f xs)

splitOn p str = mapTail tail (groupBy (const (not.p)) str)

main = do
        cmds <- fmap (splitOn (==";")) getArgs
        
        (putStr, flushOut) <- stmOut stdout
        (putErr, flushErr) <- stmOut stderr
        
        outChannels     <- newRef M.empty :: IO (TVar (M.Map ThreadId (TChan String)))
        
        go <- newRef False :: IO (TVar Bool)
        
        let     startCmd []     = do
                        atomically $ putErr "Empty command\n"
                        newRef True
                startCmd cmd    = do
                        done <- newRef False
                        
                        thread <- forkIO $ flip finally (writeRef done True) $ do
                                atomically (waitForTrue (readRef go))
                                
                                runCmd cmd
                        
                        ch <- newChannel
                        modifyRef outChannels (M.insert thread ch)
                        
                        return done
                
                mkInPipe = do
                        (r,w) <- createPipe
                        r <- fdToHandle r
                        w <- fdToHandle w
                        
                        hSetBuffering r NoBuffering
                        hSetBuffering w NoBuffering
                        
                        me <- myThreadId
                        myInputs <- fmap (M.elems . M.delete me) (readRef outChannels)
                        myInputs <- mapM dupChannel myInputs
                        
                        (output, fl) <- stmOut w
                        
                        forkIO $ atomLoop $ do
                                str <- foldl1 orElse [readChannel ch | ch <- myInputs]
                                output str
                        
                        return (r, fl)
                
                mkOutPipe = do
                        (r,w) <- createPipe
                        r <- fdToHandle r
                        w <- fdToHandle w
                        
                        hSetBuffering r NoBuffering
                        hSetBuffering w NoBuffering
                        
                        me <- myThreadId
                        myOutput <- fmap (M.! me) (readRef outChannels)
                        
                        input <- stmInUnbuf r
                        
                        forkIO $ atomLoop $ do
                                str <- input
                                        
                                writeChannel myOutput [str]
                        
                        return w

                
                runCmd (cmd:args) = do
                        (inPipe, f) <- mkInPipe
                        outPipe <- mkOutPipe
                        
                        ph <- runProcess cmd args Nothing Nothing (Just inPipe) (Just outPipe) (Just stderr)
                        
                        waitForProcess ph
                        return ()
        
        done <- mapM startCmd cmds :: IO [TVar Bool]
        
        writeRef go True
        atomically $ waitForTrue (allM readRef done)
        
        flushOut
        flushErr