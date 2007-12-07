{-# OPTIONS_GHC -fglasgow-exts #-}

module BFCell (
		BFCell(..)
	) where


class (Monad m, Num b, Ord b) => BFCell m a b | m a -> b
	where
		inc :: a -> m ()
		inc = incNTimes 1
		dec :: a -> m ()
		dec = incNTimes (-1)
		isZero :: a -> m Bool
		isZero = isN 0
		
		output :: a -> m b
		input :: a -> b -> m ()
		
		-- optimized forms, with default implementations
		incNTimes :: b -> a -> m ()
		incNTimes n cell
			| n < 0		= do {dec cell; incNTimes (n + 1) cell}
			| n == 0	= do {return ()}
			| n > 0		= do {inc cell; incNTimes (n - 1) cell}
		
		isN :: b -> a -> m Bool
		isN n cell
			| n < 0		= do {inc cell; ans <- (isN (n + 1) cell); dec cell; return ans}
			| n == 0	= isZero cell
			| n > 0		= do {dec cell; ans <- (isN (n - 1) cell); inc cell; return ans}
