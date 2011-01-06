-- module BWT2 (bwt) where
-- 
-- import Data.List 
-- 
-- rotate :: Int -> [x] -> Int -> [x] 
-- rotate l xs n = (drop (l-n) xs) ++ (take (l-n) xs) 
-- 
-- bwt xs = 
--   let l  = length xs 
--       ys = rotate l xs 
--   in  map (last . rotate l xs) $ 
--       sortBy (\n m -> compare (ys n) (ys m)) [0..(l-1)] 

-- module BWT3 (bwt) where 
-- 
-- import Data.List 
-- import qualified Data.ByteString as Raw 
-- 
-- rotate :: Int -> Raw.ByteString -> Int -> Raw.ByteString 
-- rotate l xs n = (Raw.drop (l-n) xs) `Raw.append` (Raw.take (l-n) xs) 
-- 
-- bwt xs = 
--   let l  = Raw.length xs 
--       ys = rotate l xs 
--   in  Raw.pack $ 
--       map (Raw.last . rotate l xs) $ 
--       sortBy (\n m -> compare (ys n) (ys m)) [0..(l-1)] 

-- module BWT4 (bwt) where 
module Main where

import System

import Control.Monad.ST
import Data.List 
import qualified Data.Monoid as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Algorithms.Intro as Intro

-- fusion of rotate with V.!; It is NEVER necessary to compute a 
-- full rotated array; doing so was the cause of most of the memory 
-- wastage of the other version.
rotateIndex l xs n i = xs V.! i'
    where
        i'  | i >= n    = i - n
            | otherwise = i - n + l

lastRotate l xs n = rotateIndex l xs n (l-1)

compareRotate l xs n1 n2 = M.mconcat
    [ compare (rotateIndex l xs n1 i) 
              (rotateIndex l xs n2 i)
    | i <- [0..l-1]
    ]

bwt xs = runST $ do
    let l = V.length xs
    
    is <- MV.new l
    sequence_ [MV.write is i i | i <- [0..(l-1)]]
    Intro.sortBy (compareRotate l xs) is
    
    is <- V.unsafeFreeze is
    return (V.map (lastRotate l xs) is)

main = do
    [f] <- getArgs
    xs <- readFile f
    let ys = bwt (V.fromList xs)
    putStrLn (V.toList ys)


-- There are significant gains yet to be had:
--  * Go back to ByteString for the file contents and use proper ByteString IO.
--  * mmap the input file
--  * use Data.Permute for 'is'?  Not sure if it actually performs better or not, 
--      but it is more specialized so perhaps it could be made to do better if
--      it doesn't already.  In any case, it could potentially make it clearer
--      what's going on.
--
