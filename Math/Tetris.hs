#!runhaskell
{-
 -	"Tetris.hs"
 -	(c) 2007 James Cook
 -
 -	Rough sketch of part of an idea for tetris region-tiling algorithm
 -}

module Math.Tetris where

import Data.Bits

type Board = ((Coord, Coord), Integer)

type Coord = Int
type Orientation = Int

class Tile a where
    pieces :: [a]
    orientations :: a -> Int
    size :: a -> Orientation -> (Coord,Coord)
    bitMask :: a -> Orientation -> Coord -> Integer

data Tiles = Square | Tee | L1 | L2 | S1 | S2 | I

instance Tile Tiles
    where
	pieces = [Square, Tee] -- , L1, L2, S1, S2, I]
	
	orientations Square = 1
	orientations Tee = 4
	orientations L1 = 4
	orientations L2 = 4
	orientations S1 = 2
	orientations S2 = 2
	orientations I = 2
	
	size Square _ = (2,2)
	size Tee orientation
	    | odd orientation	= (3,2)
	    | otherwise		= (2,3)
	size L1 orientation
	    | odd orientation	= (3,2)
	    | otherwise		= (2,3)
	size L2 orientation
	    | odd orientation	= (3,2)
	    | otherwise		= (2,3)
	size S1 1 = (3,2)
	size S1 2 = (2,3)
	size S2 1 = (3,2)
	size S2 2 = (2,3)
	size I 1 = (1,4)
	size I 2 = (4,1)
	
	bitMask Square _ width = 3 .|. (3 `shiftL` width)
	bitMask Tee 1 width = 7 .|. (2 `shiftL` width)
	bitMask Tee 2 width = 1 .|. (3 `shiftL` width) .|. (1 `shiftL` width `shiftL` width)
	bitMask Tee 3 width = 2 .|. (7 `shiftL` width)
	bitMask Tee 1 width = 2 .|. (3 `shiftL` width) .|. (2 `shiftL` width `shiftL` width)
	-- ...

place :: (Tile a) => Board -> (Coord, Coord) -> [((a, Orientation), Board, (Coord, Coord))]
place = undefined

tilings :: (Tile a) => (Coord, Coord) -> [[(a, Orientation)]]
tilings = undefined