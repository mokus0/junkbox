module Functions.Interleave where

import Data.Bits
import Data.Word
import qualified Data.Vector.Unboxed as U

bs = U.fromList
    [ (1, 0x55555555)
    , (2, 0x33333333)
    , (4, 0x0F0F0F0F)
    , (8, 0x00FF00FF)
    ]

interleave :: Word16 -> Word16 -> Word32
interleave evens odds = x .|. shiftL y 1
    where
        f (s, b) x = (x .|. shiftL x s) .&. b
        
        x = U.foldr' f (fromIntegral evens) bs
        y = U.foldr' f (fromIntegral odds)  bs

uninterleave :: Word32 -> (Word16, Word16)
uninterleave z = (fromIntegral evens, fromIntegral odds)
    where
        f x (s, b) = 
            let bx = (x .&. b)
             in bx .|. shiftR bx s
        
        evens = U.foldl' f         z    bs
        odds  = U.foldl' f (shiftR z 1) bs