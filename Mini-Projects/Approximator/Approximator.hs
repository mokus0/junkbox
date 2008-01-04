{-# OPTIONS -fglasgow-exts #-}
{-
 -	"Approximator.hs"
 -	(c) 2008 James Cook
 -}

module Approximator (summarize, Summary(..)) where

import Data.Ratio
import Data.Array
import Control.Concurrent.STM
import Random

class (Ix i) => Summary s i | s -> i where
	threadsafe :: s -> Bool
	threadsafe _ = False
	
	new :: (i,i) -> IO s
	tally :: s -> i -> Bool -> IO ()
	readout :: (Fractional b) => s -> i -> IO b

stmSummarize :: (Ix i)
	=> (a -> Bool)			-- function to summarize
	-> IO a				-- input-value generator
	-> (a -> i)			-- input-value bin selector
	-> (i, i)			-- bin range
	-> IO (IO (), a -> IO Bool)	-- ticker and approximation
stmSummarize f gen bin range = do
     	-- lexically scoped type variables would allow me to drop this line...
	let summarize' = summarize :: (Ix i) => (a -> Bool) -> IO a -> (a -> i) -> (i, i) -> IO (Array i (TVar (Integer, Integer)), IO (), a -> IO Bool)
	(s, step, approx) <- summarize' f gen bin range
	return (step, approx)


summarize :: (Ix i, Summary s i)
	=> (a -> Bool)			-- function to summarize
	-> IO a				-- input-value generator
	-> (a -> i)			-- input-value bin selector
	-> (i, i)			-- bin range
	-> IO (s, IO (), a -> IO Bool)	-- empty summary, ticker, and approximation
summarize f gen bin binRange = do
	summary <- new binRange
	return (summary, summarizeTick f gen bin summary, playback summary bin)

summarizeTick :: (Ix i, Summary s i)
	=> (a -> Bool)			-- function to summarize
	-> IO a				-- input-value generator
	-> (a -> i)			-- input-value bin selector
	-> s				-- summary to tick
	-> IO ()			-- ticker action
summarizeTick f gen bin summary = do
	input <- gen
	let value = f input
	tally summary (bin input) value


playback :: (Ix i, Summary s i)
	=> s				-- the summary
	-> (a -> i)			-- the input value bin selector
	-> (a -> IO Bool)		-- the (approximation) function
playback summary bin input = do
	p <- readout summary (bin input)
	replay p

replay :: Double -> IO Bool
replay p
	| p <= 0	= return False
	| p >= 1	= return True
	| otherwise	= do
		x <- randomRIO (0,1)
		return (x <= p)

instance (Ix i, Integral a) => Summary (Array i (TVar (a, a))) i where
	threadsafe _ = True
	
	new bounds = do
		cells <- mapM (const (atomically $ newTVar (0,0))) (range bounds)
		return (listArray bounds cells)
	
	tally summary index hit = atomically $ do
		let cell = summary ! index
		(count, hits) <- readTVar cell
		let newCount = count + 1
		let newHits = if hit then hits + 1 else hits
		newCount `seq` newHits `seq` writeTVar cell (newCount, newHits)
	
	readout summary index = atomically $ do
		let cell = summary ! index
		(count, hits) <- readTVar cell
		if count == 0
			then return 0.5
			else do
				let prob = toInteger hits % toInteger count
				return (fromRational prob)
