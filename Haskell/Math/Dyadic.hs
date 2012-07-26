{-# LANGUAGE ViewPatterns #-}
module Math.Dyadic where

import Data.Bits
import Functions.BitFiddling

data Dyadic = Dyadic !Integer !Int
    deriving Show

-- raise exponent, truncating mantissa.  Can lower by "raising" by a 
-- negative amount.
-- WARNING: raising exponent can lose precision
-- TODO: check for over/underflow of exponent
raiseExp dE (Dyadic m e) = Dyadic (m `shiftR` dE) (e + dE)
lowerExp dE (Dyadic m e) = Dyadic (m `shiftL` dE) (e - dE)
setExp   e' (Dyadic m e) = (m `shiftR` (e' - e))

normalize   (Dyadic 0 _) = Dyadic 0 0
normalize n@(Dyadic m _) = raiseExp dE n
    where
        dE  = ffo m

withMatchingExponent f x@(Dyadic xM xE) y@(Dyadic yM yE) =
    f (setExp e x) (setExp e y) e
        where 
            e = min xE yE

instance Eq Dyadic where
    (normalize -> Dyadic xM xE) == (normalize -> Dyadic yM yE)
        = (xM, xE) == (yM, yE)

instance Ord Dyadic where
    compare = withMatchingExponent $ \mX mY _ ->
        compare mX mY

instance Num Dyadic where
    fromInteger n = Dyadic n 0
    
    (+) = withMatchingExponent $ \mX mY e ->
        normalize $ Dyadic (mX + mY) e
    
    Dyadic xM xE * Dyadic yM yE = normalize $
        Dyadic (xM * yM) (xE + yE)
    
    negate (Dyadic m e) = Dyadic (negate m) e
    
    signum (Dyadic m _) = Dyadic (signum m) 0
    abs    (Dyadic m e) = Dyadic (abs    m) e

instance Real Dyadic where
    toRational (Dyadic m e) = toRational m * 2 ^^ e

