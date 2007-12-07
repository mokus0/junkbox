	{-
	 -  <FourCell.hs>
	 -    Copyright 2007 James Cook - All Rights Reserved.
	 -}

module Life.FourCell where

import Life.Util
import Foreign

foreign import ccall "FourCell.h initializeFourCellEngine" initializeFourCellEngine :: IO ()
foreign import ccall "FourCell.h stepFourCell" stepFourCell' :: Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 -> Int16 

foreign import ccall "FourCell.h wrappingNeighborMask" wrappingNeighborMask' :: Int32 -> Int16

wrappingNeighborMask = FourCell . wrappingNeighborMask'

newtype FourCell = FourCell Int16

instance Show FourCell where
	show (FourCell x) = concatMap (showLine x) [3,2..0]
		where
			showLine x n = (map (showCell x n) [0..3]) ++ "\n"
			showCell x i j = bitSet ((i * 4) + j) x
			bitSet b x = if even ((toInteger x) `div` (2^b)) then '0' else '1'


stepFourCell
	(FourCell n, FourCell ne, FourCell e, FourCell se, FourCell s, FourCell sw, FourCell w, FourCell nw)
	(FourCell cell)
	= FourCell (stepFourCell' cell n ne e se s sw w nw)

stepFourCellSolo = stepFourCell (z,z,z,z,z,z,z,z)
	where z = FourCell 0
