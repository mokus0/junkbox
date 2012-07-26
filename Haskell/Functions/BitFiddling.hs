module Functions.BitFiddling where

import Data.Bits
import Data.Word
import qualified Data.Vector as U

ffo32 :: Word32 -> Int
ffo32 v = xs U.! fromIntegral (((v .&. negate v) * 0x077CB531) `shiftR` 27)
    where
        xs = U.fromList
            [  0,  1, 28,  2, 29, 14, 24, 3
            , 30, 22, 20, 15, 25, 17,  4, 8
            , 31, 27, 13, 23, 21, 19, 16, 7
            , 26, 12, 18,  6, 11,  5, 10, 9]

ffo :: Integer -> Int
ffo m
    | q /= 0    = ffo32 q
    | r /= 0    = 32 + ffo r
    | otherwise = error "ffo 0 is undefined"
    where
        q = fromInteger m :: Word32
        r = m `shiftR` 32

