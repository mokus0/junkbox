{-# LANGUAGE ViewPatterns #-}
module Math.Polynomial.Type 
    ( Endianness(..)
    , Poly, poly, polyCoeffs
    , polyIsZero
    ) where

import Data.List.Extras.LazyLength
import Data.Monoid

-- dropEnd p = reverse . dropWhile p . reverse
dropEnd p = go id
    where
        go t (x:xs)
            -- if p x, stash x (will only be used if 'not (any p xs)')
            | p x       =        go (t.(x:))  xs
            -- otherwise insert x and all stashed values in output and reset the stash
            | otherwise = t (x : go  id       xs)
        -- at end of string discard the stash
        go t [] = []

trim p@(Poly _ True _) = p
trim p@(Poly LE _ cs) = Poly LE True (dropEnd   (==0) cs)
trim p@(Poly BE _ cs) = Poly BE True (dropWhile (==0) cs)

-- |Make a 'Poly' from a list of coefficients using the specified coefficient order.
poly :: Num a => Endianness -> [a] -> Poly a
poly end cs = trim (Poly end False cs)

-- |Get the coefficients of a a 'Poly' in the specified order.
polyCoeffs :: Num a => Endianness -> Poly a -> [a]
polyCoeffs end p = case trim p of
    Poly e _ cs | e == end  -> cs
                | otherwise -> reverse cs

polyIsZero :: Num a => Poly a -> Bool
polyIsZero = null . coeffs . trim

data Endianness 
    = BE 
    -- ^ Big-Endian (head is highest-order term)
    | LE
    -- ^ Little-Endian (head is const term)
    deriving (Eq, Ord, Enum, Bounded, Show)

data Poly a = Poly 
    { endianness :: !Endianness
    , trimmed :: !Bool
    , coeffs :: ![a]
    }

instance Num a => Show (Poly a) where
    showsPrec p (trim -> Poly end _ cs) 
        = showParen (p > 10) 
            ( showString "poly "
            . showsPrec 11 end
            . showChar ' '
            . showsPrec 11 cs
            )

instance (Num a, Eq a) => Eq (Poly a) where
    p == q  
        | endianness p == endianness q
        = coeffs (trim p) == coeffs (trim q)
        | otherwise 
        = polyCoeffs BE p == polyCoeffs BE p
        

-- instance (Num a, Ord a) => Ord (Poly a) where
--     compare p q = mconcat
--             [ lengthCompare pCoeffs qCoeffs
--             , compare       pCoeffs qCoeffs
--             ]
--         where
--             pCoeffs = polyCoeffs BE p
--             qCoeffs = polyCoeffs BE q

instance Functor Poly where
    fmap f (Poly end _ cs) = Poly end False (map f cs)
