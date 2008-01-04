{-
 -	"/Users/mokus/Projects/Approximator/Main.hs"
 -	(c) 2008 James Cook
 -}

module Main where

import Control.Monad
import Data.Array
import Approximator
import Random
import Control.Concurrent
import Control.Concurrent.STM

test' iters threads = test iters threads (\x -> even (floor (id (x ** pi)))) (randomRIO (0, 1000)) (\x -> floor (x / 10)) (0,100)

test iters threads f gen bin range = do
	threadEvents <- newTChanIO :: IO (TChan (Either ThreadId ThreadId))
	
	let up =   do t <- myThreadId; atomically $ writeTChan threadEvents (Left t)
	let down = do t <- myThreadId; atomically $ writeTChan threadEvents (Right t)
	
	(summary, step, eval) <- summarize f gen bin range :: IO (Array Int (TVar (Integer, Integer)), IO (), Double -> IO Bool)
	replicateM threads (forkOS (do up; replicateM (iters `div` threads) step; down))
	
	return (threadEvents, summary, step, eval, readout summary)

repeatM x = x >> repeatM x

main = do
	births <- newTVarIO 0
	deaths <- newTVarIO 0
	
	(events, summary, step, eval, rs) <- test' 100000 2
	
	atomically $ do
		Left _ <- readTChan events
		incr births
	
	while (atomically $ readTVar births `mgt` readTVar deaths) $ atomically $ do
		event <- readTChan events
		case event of
			Left _ -> incr births
			Right _ -> incr deaths
	
	mapM_ (\x -> rs x >>= print) (range $ bounds summary)


incr v = modify v (+1)
decr v = modify v (subtract 1)

modify v f = do
	x <- readTVar v
	writeTVar v (f x)

a `mgt` b = do
	a <- a
	b <- b
	return (a > b)

while cond loop = do
	c <- cond
	if c
		then do
			loop
			while cond loop
		else
			return ()