module Math.RomanNumerals2 where

data Sym = I | V | X | L | C | D | M
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data RN = Sym !Sym | Sum [RN] | Diff !RN !RN

normalize :: RN -> RN
normalize = undefined