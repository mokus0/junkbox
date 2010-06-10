module BF (
		runBF
	) where

import OptBF
import BFTape

import Control.Monad.ST
import Data.Int
import Data.STRef

{-# SPECIALIZE runBF :: String -> Int -> (Int8 -> IO Int8) -> (Int8 -> IO ()) -> IO ()#-}

runBF :: (Num a, Ord a) => 
	   String		-- the program
	-> Int			-- the size of the tape to allocate
	-> (a -> IO a)		-- an input function; arg is current cell value
	-> (a -> IO ()) 	-- an output function
	-> IO ()		-- returns: ()
	
runBF prog tapeSize inF outF = unsafeSTToIO (runBF_ST prog tapeSize (unsafeIOToST . inF) (unsafeIOToST . outF))

{-# SPECIALIZE runBF_ST :: String -> Int -> (Int8 -> ST s Int8) -> (Int8 -> ST s ()) -> ST s ()#-}

runBF_ST :: (Num a, Ord a) =>
	   String		-- the program
	-> Int			-- the size of the tape to allocate
	-> (a -> ST s a)	-- an input function; arg is current cell value
	-> (a -> ST s ()) 	-- an output function
	-> ST s ()		-- returns: ()

runBF_ST prog tapeSize inF outF = do
	tape <- newTape tapeSize 0
	
	runOptBF (read prog) tape inF outF
