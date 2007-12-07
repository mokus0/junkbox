module CoreBF (
		CoreBF(..),
		charToCoreBF, coreBFToChar
	) where

data CoreBF = 
	  Inc | Dec
	| GoL | GoR
	| Beg | End
	| Inp | Out
	| Nop Char	-- an extra no-op constructor, to simplify parsing
	deriving (Eq)

charToCoreBF '+' = Inc
charToCoreBF '-' = Dec
charToCoreBF '<' = GoL
charToCoreBF '>' = GoR
charToCoreBF '[' = Beg
charToCoreBF ']' = End
charToCoreBF ',' = Inp
charToCoreBF '.' = Out
charToCoreBF  c  = Nop c

coreBFToChar Inc		= '+'
coreBFToChar Dec		= '-'
coreBFToChar GoL		= '<'
coreBFToChar GoR		= '>'
coreBFToChar Beg		= '['
coreBFToChar End		= ']'
coreBFToChar Inp		= ','
coreBFToChar Out		= '.'
coreBFToChar (Nop c)	=  c 

instance Read CoreBF
	where
		readsPrec _ [] = []
		readsPrec _ (c:cs) = [(charToCoreBF c, cs)]

instance Show CoreBF
	where
		show x = [coreBFToChar x]