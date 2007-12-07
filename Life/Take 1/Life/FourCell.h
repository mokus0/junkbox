	/*
	 *  <FourCell.h>
	 *    Copyright 2007 James Cook - All Rights Reserved.
	 */

#ifndef __FourCell_h__
#define __FourCell_h__
	
		// define this to skip the initialization check
		// in stepFourCell - requires an explicit
		// call to initializeFourCellEngine
	#undef RequireExplicitInitializationCall
	
	/* typedef Int16 FourCell */
	typedef unsigned short FourCell;
	
		/*
		 *  Cell mask macros
		 */
	#define CellMask(cell)					(1 << (cell))

	#define RowMask(cell)					(0xf << ((cell) & 0xC))
	#define ColMask(cell)					((1 << ((cell) & 0x3)) * 0x1111)
	
	#define RowMask3(cell)					(((CellMask(cell) << 1) | CellMask(cell) | (CellMask(cell) >> 1)) & RowMask(cell))
	#define BlockMask3(cell)				((RowMask3(cell) << 4) | RowMask3(cell) | (RowMask3(cell) >> 4))
	#define NeighborMask(cell)				(0xffff & BlockMask3(cell) & (~CellMask(cell)))
	
	#define WrappingBlockMask(cell)			(~RowMask((cell) ^ 0xA) & ~ColMask((cell) ^ 0xA))
	#define WrappingNeighborMask(cell)		(0xffff & WrappingBlockMask(cell) & (~CellMask(cell)))
	
		// initializeFourCellEngine :: IO ()
	void initializeFourCellEngine();

		// lifeRule' :: Int32 -> Int32 -> Int32
	int lifeRule(int living, int neighborsLiving);

		// countBits' :: Int16 -> Int32
	int countBits(FourCell cell);

		// cellMask' :: Int32 -> Int16
	FourCell cellMask(int cellnum);

		// neighborMask' :: Int32 -> Int16
	FourCell neighborMask(int cellnum);
	FourCell wrappingNeighborMask(int cellnum);

		// stepFourCell' :: Int16 -> Int16 -> Int16 -> Int16
		//      -> Int16 -> Int16 -> Int16 -> Int16
		//      -> Int16
	FourCell stepFourCell(
			FourCell cell,
			FourCell N,
			FourCell NE,
			FourCell E,
			FourCell SE,
			FourCell S,
			FourCell SW,
			FourCell W,
			FourCell NW
		);
	
	FourCell computeCenterBlock(FourCell cell);
	FourCell computeWrappingBlock(FourCell cell);
	
	
#endif /* __FourCell_h__ */
