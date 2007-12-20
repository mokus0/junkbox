	{-
	 -  <Life/Util.hs>
	 -    Copyright 2007 James Cook - All Rights Reserved.
	 -}

module Life.Util where

class EnumAll a where
	enumAll :: [a]

	-- 2d neighbor directions
data Neighbor2D = N | NE | E | SE | S | SW | W | NW
	deriving (Eq, Show, Read, Enum)

instance EnumAll Neighbor2D where
	enumAll = [N .. NW]
