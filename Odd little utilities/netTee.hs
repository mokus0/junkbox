#!/usr/bin/env runhaskell
{-
 -      ``netTee''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Main where

import System.Environment
-- import Network (listenOn, accept, PortID(PortNumber))
import Network.BSD
import Network.Socket -- hiding (accept)
import Data.Bits
import Data.Word

import qualified Data.Map as M

import Data.StateRef
import Data.Channel

import Control.Monad.Loops
import Control.Exception

import Control.Concurrent

import Util.STMIO
import IO

import Text.Printf
import Data.Char

recvSz = maxListenQueue -- 16

-- on my computer the endianness is wrong in the internal handling of
-- the PortNumber type
portNum :: Word16 -> PortNumber
portNum b = PortNum (rotateL b 8)

listenOn port = do
        tcp <- getProtocolNumber "tcp"
        bracketOnError
                (socket AF_INET Stream tcp)
                (sClose)
                (\sock -> do
                        setSocketOption sock ReuseAddr 1
                        bindSocket sock (SockAddrInet (portNum port) iNADDR_ANY)
                        listen sock maxListenQueue
                        return sock
                )

startListening dump buffers port = do
        done  <- newRef False
        
        forkIO $ (`finally` writeRef done True) $ do
                s <- listenOn port
                (c, addr) <- accept s
                sClose s
                
                let     until b x = while (fmap not (readRef b)) x `finally` writeRef done True
                        myChannel = buffers M.! port
                        outChannels = M.elems (M.delete port buffers)
                
                forkIO $ until done $ do
                        -- listener
                        str <- recv c recvSz
                        atomically $ sequence $ dump port str :
                                [ writeChannel ch str
                                | ch <- outChannels
                                ]
                
                forkIO $ until done $ do
                        -- speaker
                        str <- readChannel myChannel
                        
                        let     sendFully [] = return ()
                                sendFully str = do
                                        sent <- send c str
                                        sendFully (drop sent str)
                        
                        sendFully str
                
                return ()
        
        return done

main = withSocketsDo $ do
        ps <- fmap (map read) getArgs :: IO [Word16]
        
        buffers <- fmap M.fromList $ sequence $ do
                p <- ps
                return $ do
                        c <- newChannel :: IO (TChan String)
                        return (p, c)
        
        (putStr, flush) <- stmOut stdout
        
        let     dump port str = putStr (printf "%6s: %s\n" (show port) (hexdump str))
                hexdump [] = ""
                hexdump (c:cs) = printf "%02x " (ord c) ++ hexdump cs
        
        done <- mapM (startListening dump buffers) ps :: IO [TVar Bool]
        
        atomically $ waitForTrue (allM readRef done)