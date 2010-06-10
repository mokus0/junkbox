module CodeMetric where

import Data.List

class CodeMetric a
	where
		measure :: a -> String -> Int

data SymbolCount = SymbolCount [Char]
	deriving (Read, Show)

instance Eq SymbolCount
	where
		(SymbolCount l1) == (SymbolCount l2) = (sort l1) == (sort l2)
		a /= b = not (a == b)

instance CodeMetric SymbolCount
	where
		measure (SymbolCount symbols) string = length (filter (`elem` symbols) string)
		
data LineCount = LineCount
	deriving (Eq, Show, Read)

instance CodeMetric LineCount
	where
		measure LineCount = measure (SymbolCount "\n")
