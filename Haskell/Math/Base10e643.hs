module Math.Base10e643 where

-- Silly little exercise: Base-10^643 natural numbers, stored in 267-byte chunks.

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits

-- little endian
newtype Number = N [ByteString]
    deriving (Eq, Show)

nBytesPerChunk  = 267
nDigitsPerChunk = 643
nBitsPerChunk   = nBytesPerChunk * 8


pack :: Integer -> Number
pack 0 = N []
pack n = case n `divMod` (10 ^ nDigitsPerChunk) of
    (d,m) -> case pack d of
        ~(N rest) -> N (chunkFromInteger m : rest)


n `splitAtBit` b =
    (n `shiftR` b, n .&. mask)
    where mask = bit b - 1

chunkFromInteger :: Integer -> ByteString
chunkFromInteger = g . BS.unfoldrN nBytesPerChunk f
    where 
        f n = case n `splitAtBit` 8 of
            (d,m) -> Just (fromIntegral m, d)
        
        g (bs, Just 0) = bs
        g (_, _) = error "chunkFromInteger: overflow"

chunkToInteger :: ByteString -> Integer
chunkToInteger = BS.foldr' f 0 . g
    where
        f m d = toInteger m + d * 256
        
        g bs 
            | BS.length bs == nBytesPerChunk    = bs
            | otherwise = error "chunkToInteger: invalid chunk size"

unpack :: Number -> Integer
unpack (N cs) = foldr f 0 cs
    where
        f d m = chunkToInteger d + m * 10^nDigitsPerChunk
