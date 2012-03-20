module Math.PiDigits where

import Data.Monoid
import Math.AddCFs
import System.IO

-- Not particularly fast, but it was pretty darn easy to implement with the existing
-- machinery in Math.AddCFs
main = do
    hSetBuffering stdout NoBuffering
    putStr piString

piString = case piDigits of
    d : ds
        -> shows d 
        .  showChar '.' 
        .  foldr (\x xs -> shows x . xs) id ds
        $ ""

piDigits :: [Int]
piDigits 
    = map fromInteger 
    $ rlfToDigitsWith (shouldEmitDigit . idRange) (mempty, piCF)
    where
        idRange = id :: CFRange Rational -> CFRange Rational
        

shouldEmitDigit (Inside (Lift x) (Lift y))
    |   y <  fromInteger (ceiling x)
    || (x == fromInteger (floor   x) && y < x+1)
    = Just (floor x)
shouldEmitDigit _ = Nothing

rlfToDigitsWith :: (RealFrac b, Integral a) => (CFRange b -> Maybe a) -> (RLF a, CF a) -> [a]
rlfToDigitsWith shouldEmit (rlf,cf) = case shouldEmit (rlfRange rlf cf) of
    Just t  -> t : (rlfToDigitsWith shouldEmit (emit t))
    Nothing -> either finish (rlfToDigitsWith shouldEmit) (stepRLF rlf cf)
    where
        emit t = (reduceRLF (emitDigitRLF t rlf), cf)
        finish (a,c) = fracToDigits a c

fracToDigits p q = error "write me! (fracToDigits)"

