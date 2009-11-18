module ZTBS where

import Data.Word
import Test.QuickCheck

-- zero-terminated byte stream encoding.
-- basic idea is that the encoded stream has a 
-- 'shift' state which propagates forward.  All
-- tokens other than 0 and 255 are 'literals' 
-- corresponding to the token's value plus the shift.
-- 0 terminates the stream and 255 increments the shift.

-- worst case under this implementation of encode is for the list
-- [0,2..x], which gives encoded length 3N+1 (where N is the length
-- of the decoded list).  Same behavior can be triggered by 
-- (255:[1,3..]) or any other list ascending by 2s that happens
-- to start with the shift state or one less than it (mod 256 of 
-- course).

encode :: [Word8] -> [Word8]
encode = go 0
    where
        go s []     = [0]
        go s (x:xs) 
            | x' `elem` [0,255]   = 255 : go (s+1) (x:xs)
            | otherwise = x'  : go s xs
            where x' = x - s

decode :: [Word8] -> [Word8]
decode = go 0
    where
        go s []     = error "end of stream encountered without zero terminator"
        go s (0  :xs) = []
        go s (255:xs) = go (s+1) xs
        go s (x:xs)   = (s + x) : go s xs

instance Arbitrary Word8 where
    arbitrary = fmap fromIntegral (choose (0,255 :: Int))
    
test str = str == decode (encode str)

