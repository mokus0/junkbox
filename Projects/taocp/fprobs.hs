{-
 -      ``fprobs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Fprobs where

import Data.Graph.Inductive
--import qualified Data.Map as M
import Data.List
import Control.Monad

fs :: Int -> [Gr Int ()]
fs n = [mkGraph [(i,i) | i <- [1..n]] (zip3 [1..n] j (repeat ())) | j <- replicateM n [1..n]]

--fs n = [zip [1..n] j | j <- replicateM n [1..n]]

cycles f = nub [sort (reachable next f) | node <- nodes f, next <- suc f node, node `elem` reachable next f]

pDegen f = ones / total
        where
                single [x] = True
                single _   = False
                
                cs = cycles f
                ones =  fromIntegral $ length (filter single cs)
                total = fromIntegral $ length (concat cs)

pDegen' fs = mean pDegen fs
        where
                mean f xs = sum [f x | x <- xs] / genericLength xs

--fixedPoints = map fst . filter (uncurry (==))
--hasFixedPoint = not . null . fixedPoints


