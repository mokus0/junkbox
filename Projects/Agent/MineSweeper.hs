#!runhaskell
{-
 -	"/Users/mokus/Desktop/Haskell/Agent/MineSweeper.hs"
 -	(c) 2007 James Cook
 -}

module MineSweeper where

import World
import Agent

import Random
import Control.Concurrent.STM
import Data.Set

type Size = (Int, Int)

type Board = (Size, Set (Int, Int))
type Marks = Set (Int, Int)

data Obs = BoardSize Size | Cell Int Int (Maybe Int) | YoureDead
	deriving (Eq, Show)
	
data Cmd = Try Int Int | Mark Int Int
	deriving (Eq, Show)

type MineSweeper st = World (Board, Marks) st Obs Cmd

mkBoard :: Size -> Int -> IO Board
mkBoard size mines = addMines (size, empty) mines

addMines :: Board -> Int -> IO Board
addMines board 0 = return board
addMines ((sx,sy), board) mines = do
	x <- randomRIO (1, sx)
	y <- randomRIO (1, sy)
	if (x,y) `member` board
		then addMines ((sx, sy), board) mines
		else addMines ((sx, sy), insert (x,y) board) (mines - 1)

mkGame :: Size -> Int -> IO (MineSweeper st)
mkGame size mines = do
	board <- mkBoard size mines
	mkWorld (board, empty) nullRule

gameState :: (MineSweeper st) -> IO (Board, Marks)
gameState game = atomically (readTChan (world game))

