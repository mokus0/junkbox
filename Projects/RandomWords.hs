{-
 -      ``RandomWords''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Main where

import Random
import GHC.IOBase
import Data.List
import Data.Ord
import qualified Data.Set as S

randomSplit xs = randomSplitR (0, length xs - 1) xs

randomSplitR posR xs = do
        pos <- randomRIO posR
        let (pre, x : post) = splitAt pos xs
        return (x, pre ++ post)

uniqueRandoms count randomThingy = gen count (S.fromList [])
        where   gen 0 xs = return xs
                gen (n+1) xs = do
                        x <- randomThingy
                        if x `S.member` xs
                                then gen (n+1) xs
                                else gen n (x `S.insert` xs)

extract ns list = extractSet (S.fromList ns) list
extractSet ns list = partitionByIndex (`S.member` ns) list

partitionByIndex p list = part 0 list [] []
        where
                part n []     outT outF = (reverse outT, reverse outF)
                part n (x:xs) outT outF
                        | p n           = part (n+1) xs (x:outT) (  outF)
                        | otherwise     = part (n+1) xs (  outT) (x:outF)

randomExtract count range list = do
        items <- uniqueRandoms count (randomRIO range)
        return (extractSet items list)

randomPermute list = perm (length list) list
        where
                chunkSize = 5000
                
                perm ct list
                        | ct > chunkSize        = do
                                (chunk, rest) <- randomExtract chunkSize (0, ct-1) list
                                chunk <- rPerm chunkSize chunk
                                rest <- unsafeInterleaveIO (perm (ct - chunkSize) rest)
                                return (chunk ++ rest)
                        | otherwise             = rPerm ct list
                
                rPerm 0 xs = return xs
                rPerm len xs = do
                        (x, rest) <- randomSplitR (0, len - 1) xs
                        rest <- unsafeInterleaveIO (rPerm (len - 1) rest)
                        return (x : rest)

randomWord lengthRange chars = do
        len <- randomRIO lengthRange :: IO Int
        
        let     nChars = length chars
                
                word 0          = return []
                word (n+1)      = do
                        ch <- randomRIO(0,nChars - 1)
                        rest <- word n
                        return ((chars !! ch) : rest)
        
        word len

letters = ['a'..'z'] ++ ['A'..'Z']

dalesRandomWord = randomWord (3,15) letters

dalesWordList = do
        wordSet <- uniqueRandoms 65000 dalesRandomWord
        randomPermute (S.toList wordSet)

main = mapM_ putStrLn =<< dalesWordList