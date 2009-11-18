{-# OPTIONS_GHC -fglasgow-exts #-}

module BFTape (
		BFTape(..),
		Tape, newTape
	) where

import BFCell

import Control.Monad.ST
import Data.Int
import Data.STRef
import Data.Array.ST

class (Monad m, BFCell m a b) => BFTape m t a b | m t -> a b
	where
		currentCell :: t -> m a
		offset :: Int -> t -> m a
		
		goLeft :: t -> m ()
		goLeft = goN 1
		goRight :: t -> m ()
		goRight = goN (-1)
		
		goN :: Int -> t -> m ()

-- a cell
data Cell s a = Cell (STRef s a)

instance (Num a, Ord a) => BFCell (ST s) (Cell s a) a
	where
		{-# SPECIALIZE instance BFCell (ST s) (Cell s Int8) Int8 #-}
		
		output (Cell cell) = readSTRef cell
		input (Cell cell) val = writeSTRef cell val

		incNTimes n (Cell cell) = do
			x <- readSTRef cell
			writeSTRef cell (x + n)
		isN n (Cell cell) = do
			x <- readSTRef cell
			return (x == n)

-- A wrapping tape with an arbitrary type cell
data Tape s a = Tape {
		tapeLen :: Int,
		tapePos :: STRef s Int,
		tapeData :: STArray s Int (Cell s a)
	}

instance (Num a, Ord a) => BFTape (ST s) (Tape s a) (Cell s a) a
	where
		{-# SPECIALIZE instance BFTape (ST s) (Tape s Int8) (Cell s Int8) Int8 #-}
		currentCell tape = do
			loc <- readSTRef (tapePos tape)
			readArray (tapeData tape) loc
		goN n tape = do
			loc <- readSTRef (tapePos tape)
			let size = tapeLen tape
			let newLoc = (loc + n) `mod` size
			writeSTRef (tapePos tape) newLoc
		offset n tape = do
			loc <- readSTRef (tapePos tape)
			let size = tapeLen tape
			readArray (tapeData tape) ((loc + n) `mod` size)

newTape :: Int -> a -> ST s (Tape s a)
newTape size defVal = do
	pos <- newSTRef 0;
	let bnds = (0, size - 1)
	array <- newArray_ bnds
	
	let setArrayAt x = do
		newRef <- newSTRef defVal
		writeArray array x (Cell newRef)
	mapM setArrayAt (range bnds)
	
	return (Tape {tapeLen = size, tapePos = pos, tapeData = array})
