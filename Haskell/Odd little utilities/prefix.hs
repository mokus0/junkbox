#!/usr/bin/env runhaskell
{-
 -      ``prefix''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Main where

import Data.Tree
import Data.Ord
import Data.List
import System.Environment

mkPrefixBranch :: String -> Forest Char
mkPrefixBranch [] = []
mkPrefixBranch (x:xs) = [Node x (mkPrefixBranch xs)]

mergePrefixBranches :: Ord a => Tree a -> Tree a -> Forest a
mergePrefixBranches n1@(Node x xs) n2@(Node y ys) = case x `compare` y of
    LT  -> [n1, n2]
    EQ  -> [Node x (mergePrefixForests xs ys)]
    GT  -> [n2, n1]

mergePrefixForests :: Ord a => Forest a -> Forest a -> Forest a
mergePrefixForests xs ys = mergeByWith (comparing rootLabel) mergePrefixBranches xs ys

mkPrefixTree :: [String] -> Forest Char
mkPrefixTree strs = foldr mergePrefixForests [] (map mkPrefixBranch strs)

insertByWith cmp (+) x [] = [x]
insertByWith cmp (+) x ys@(y:ys')
    = case cmp x y of
        GT -> y : insertByWith cmp (+) x ys'
        EQ -> (x + y) ++ ys'
        LT -> x : ys

mergeByWith cmp (+) [] ys = ys
mergeByWith cmp (+) (x:xs) ys = mergeByWith cmp (+) xs (insertByWith cmp (+) x ys)

readout :: Forest Char -> String
readout [] = ""
readout [Node c rest] = c : readout rest
readout ns = "{" ++ concat (intersperse "," (map (readout.(:[])) ns)) ++ "}"

main = getArgs >>= putStrLn . readout . mkPrefixTree
-- TODO : handle eclipsed endings - eg, ["ab", "abc"]
-- TODO : optimize output for length - eg, consider input: {1,2,3,4}{1,2,3,4}{1,2,3,4}{1,2,3,4}