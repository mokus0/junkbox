{-
 -      ``Scramble''
 -      (c) 2008 James Cook
 -}

module Scramble where

import Data.Graph.Inductive
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

mkBoard :: (Graph gr, Integral a) => ((a,a) -> b) -> a -> gr b ()
mkBoard f n = mkGraph 
        nodeList
        [ (from, to, ())
        | fromCell@(x,y) <- cells
        , toCell <- [(x + i, y + j) | i <- [-1..1], j <- [-1..1], or [x /= 0, y /= 0]]
        , from <- M.lookup fromCell nodeMap
        , to <- M.lookup toCell nodeMap
        ]
        where
                cells :: (Num a, Enum a) => [(a, a)]
                cells = [(x,y) | x <- [1..fromIntegral n], y <- [1..fromIntegral n]]
                
                nodeList = zip [1..] (map f cells)
                
                --nodeMap = M.fromList nodeList
                nodeMap = M.fromList (zip cells [1..])

mkRawBoard :: (Graph gr, Integral a) => a -> gr (a,a) ()
mkRawBoard = mkBoard id

mkListBoard :: (Graph gr) => [[b]] -> gr b ()
mkListBoard lists = mkBoard (\(x+1,y+1) -> lists !! x !! y) (length lists)

containsPath :: (Ord a, DynGraph gr) => gr a b -> [a] -> Bool
gr `containsPath` [] = True
gr `containsPath` path@(x:xs) = or $ do
        let graph = filterGraph (`elem` (nub path)) gr
        return undefined

gSelectAll :: (DynGraph gr) => gr a b -> [GDecomp gr a b]
gSelectAll gr
        | isEmpty gr    = []
        | otherwise     = decomp:decomps
        where
                decomp@(cxt@(toCxt,cxtNode,fromCxt), rest) = matchAny gr
                decomps = map repair (gSelectAll rest)
                repair (subCxt@(toSub,subNode,fromSub),subGr) = (relinked, unlinked & gr)
                        where
                                unlinked = (filter (/= subNode) toCxt, cxtNode, filter (/= subNode) fromCxt)
                                relinked = case (subNode `elem` toCxt, subNode `elem` fromCxt) of
                                        (True,  True )    -> ((cxtNode:toSub,cxtNode), undefined)
                                        (True,  False)    -> (undefined,undefined)
                                        (False, True )    -> (undefined,undefined)
                                        (False, False)    -> (subCxt,cxt)
                                        
                

gSelect :: (Graph gr) => (a -> Bool) -> gr a b -> [GDecomp gr a b]
gSelect = undefined

filterGraph :: (Ord a, DynGraph gr) => (a -> Bool) -> gr a b -> gr a b
filterGraph f gr = delMapNodes nodeMap extras gr
        where   nodeMap = fromGraph gr
                extras = nub (filter f (map snd (labNodes gr)))

