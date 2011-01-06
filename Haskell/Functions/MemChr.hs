module Functions.MemChr where

import Data.ByteString as BS hiding (count)
import Data.Word

count' :: Word8 -> ByteString -> Int
count' c s = loop 0 0
    where
        loop n i
            | n >= BS.length s  = i
            | index s i == c    = loop (n+1) (i+1)
            | otherwise         = loop  n    (i+1)
