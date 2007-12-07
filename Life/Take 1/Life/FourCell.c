	/*
	 *  <FourCell.c>
	 *    Copyright 2007 James Cook - All Rights Reserved.
	 *
	 *  Build (on Mac OS X) using:
	 *    gcc -dynamiclib -o libFourCell.dylib FourCell.c
	 *
	 *  Load GHCi (on Mac OS X) using:
	 *    ghci -fglasgow-exts -L. -lFourCell Life
	 */

#include <stdio.h>
#include "FourCell.h"

static int initialized = 0;


#define LookupTableSize		(0xffff + 1)

static FourCell centerBlockLookup[LookupTableSize];
static FourCell wrappingBlockLookup[LookupTableSize];

void initializeFourCellEngine() {
	FourCell cell;
	
	centerBlockLookup[0xffff] = computeCenterBlock(0xffff);
	wrappingBlockLookup[0xffff] = computeWrappingBlock(0xffff);
	
	for (cell = 0; cell < 0xffff; cell++) {
		centerBlockLookup[cell] = computeCenterBlock(cell);
		wrappingBlockLookup[cell] = computeWrappingBlock(cell);
	}
	
	initialized = 1;
}

FourCell neighborMask(int cellnum) {
	return NeighborMask(cellnum);	
}

FourCell wrappingNeighborMask(int cell) {
	return WrappingNeighborMask(cell);
}

FourCell cellMask(int cellnum) {
	return CellMask(cellnum);	
}

int countBits(FourCell cell) {
	int bits = 0;
	
	while (cell !=0) {
		bits += cell & 1;
		
		cell >>= 1;
	}
	
	return bits;
}

#define computeCell(source, cellnum)			(lifeRule((source) & CellMask(cellnum), countBits((source) & NeighborMask(cellnum))))
#define computeWrappingCell(source, cellnum)	(lifeRule((source) & CellMask(cellnum), countBits((source) & WrappingNeighborMask(cellnum))))

FourCell computeCenterBlock(FourCell cell) {
	FourCell result = 0x0000;
	
	if (computeCell(cell, 0x5)) result |= CellMask(0x5);
	if (computeCell(cell, 0x6)) result |= CellMask(0x6);
	if (computeCell(cell, 0x9)) result |= CellMask(0x9);
	if (computeCell(cell, 0xA)) result |= CellMask(0xA);
		
	return result;
}

FourCell computeWrappingBlock(FourCell cell) {
	FourCell result = 0x0000;
	
	int c;
	
	for (c = 0; c < 16; c++) {
		if (computeWrappingCell(cell, c))
			result |= CellMask(c);
	}
		
	return result;
}

int lifeRule(int living, int neighborsLiving) {
	if (living) {
		return (neighborsLiving == 2) || (neighborsLiving == 3);
	} else {
		return neighborsLiving == 3;
	}
}

static inline FourCell computeEdge(FourCell cell0, FourCell cell1, FourCell select, FourCell filter) {
	cell0 &= ~select;
	cell1 &=  select;
	
	cell0 |= cell1;
	
	return wrappingBlockLookup[cell0] & filter;
}

static inline FourCell computeCorner (FourCell cell0, FourCell vNeighbor, FourCell cNeighbor, FourCell hNeighbor, int cell) {
	FourCell cell0Mask = BlockMask3(cell);
	FourCell hNeighborMask = CellMask (cell ^ 0x3) | CellMask (cell ^ 0x7);
	FourCell vNeighborMask = CellMask (cell ^ 0xC) | CellMask (cell ^ 0xD);
	FourCell cNeighborMask = CellMask (cell ^ 0xF);
	
	cell0 &= cell0Mask;
	cell0 |= hNeighbor & hNeighborMask;
	cell0 |= vNeighbor & vNeighborMask;
	cell0 |= cNeighbor & cNeighborMask;
	
	return wrappingBlockLookup[cell0] & CellMask(cell);
}

FourCell stepFourCell(
		FourCell cell,
		FourCell N, FourCell NE, FourCell E, FourCell SE,
		FourCell S, FourCell SW, FourCell W, FourCell NW
	) {
	#ifndef RequireExplicitInitializationCall
	if (!initialized)
		initializeFourCellEngine();
	#endif
	
	// center cells [5,6,9,A]
	FourCell result = centerBlockLookup[cell];
	
	// S edge cells [1,2]
	result |= computeEdge(cell, S, RowMask(0xC), CellMask(0x1) | CellMask(0x2));
	
	// N edge cells [D,E]
	result |= computeEdge(cell, N, RowMask(0x0), CellMask(0xD) | CellMask(0xE));
	
	// W edge cells [4,8]
	result |= computeEdge(cell, W, ColMask(0x3), CellMask(0x4) | CellMask(0x8));
	
	// E edge cells [7,B]
	result |= computeEdge(cell, E, ColMask(0x0), CellMask(0x7) | CellMask(0xB));
	
	// corner cells [0,3,C,F]
	result |= computeCorner(cell, S, SW, W, 0x0);
	result |= computeCorner(cell, S, SE, E, 0x3);
	result |= computeCorner(cell, N, NW, W, 0xC);
	result |= computeCorner(cell, N, NE, E, 0xF);
	
	return result;
}