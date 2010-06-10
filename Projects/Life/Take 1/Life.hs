module Life where

import Life.FourCell

-- some test data

testCell = FourCell 0x7d00
testNeighborhood = (
		{-N-}  FourCell 0x0002,
		{-NE-} FourCell 0x0000,
		{-E-}  FourCell 0x0000,
		{-SE-} FourCell 0x0000,
		{-S-}  FourCell 0x0000,
		{-SW-} FourCell 0x0000,
		{-W-}  FourCell 0x0000,
		{-NW-} FourCell 0x0000)


test n = mapM_ print $ take n testCellEvolution

testCellEvolution = testCell : (zipWith stepFourCell testNeighborhoodEvolution testCellEvolution)
testNeighborhoodEvolution = testNeighborhood : (zipWith stepNeighborhood testNeighborhoodEvolution testCellEvolution)

stepNeighborhood (n, ne, e, se, s, sw, w, nw) cell = (
		stepFourCell (z,z,ne,e,cell,w,nw,z)	n,
		stepFourCell (z,z,z,z,e,cell,n,z)	ne,
		stepFourCell (ne,z,z,z,se,s,cell,n)	e, 
		stepFourCell (e,z,z,z,z,z,s,cell)	se, 
		stepFourCell (cell,e,se,z,z,z,sw,w)	s, 
		stepFourCell (w,cell,s,z,z,z,z,z)	sw, 
		stepFourCell (nw,n,cell,s,sw,z,z,z)	w, 
		stepFourCell (z,z,n,cell,w,z,z,z)	nw 
	)
		where
			z = FourCell 0