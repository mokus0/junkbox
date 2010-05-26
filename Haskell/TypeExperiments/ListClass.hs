{-# OPTIONS -fglasgow-exts #-}
{-
 -      "ListClass.hs"
 -      (c) 2008 James Cook
 -      
 -      This would be nifty if original : and [] could be redefined.
 -}

module TypeExperiments.ListClass where

import qualified Data.ByteString as B
import Data.Word

class List l where
    type Elem l
    cons :: Elem l -> l -> l
    nil  :: l
    match :: (Elem l -> l -> t) -> t -> l -> t
    match f z l = case decons l of
        Nothing -> z
        Just (h,t) -> f h t
    decons :: l -> Maybe (Elem l, l)
    decons = match (\h t -> Just (h,t)) Nothing

instance List [x] where
    type Elem [x] = x
    cons = (:)
    nil = []
    match f z [] = z
    match f z (h:t) = f h t

instance List B.ByteString where
    type Elem B.ByteString = Word8
    cons = B.cons
    nil = B.empty
    decons bs
        | B.null bs = Nothing
        | otherwise = Just (B.head bs, B.tail bs)

-- class List h t l | h t -> l, l -> h t where 
--     cons     :: h -> t -> l
--     nil      :: l
--     
--     match :: (h -> t -> x) -> x -> l -> x
--     match f z l = case decons l of
--         Nothing -> z
--         Just (h,t) -> f h t
--     decons   :: l -> Maybe (h,t)
--     decons = match (\h t -> Just (h,t)) Nothing
--             
-- instance List x [x] [x] where
--     cons = (:)
--     nil = []
--     decons [] = Nothing
--     decons (h:t) = Just (h,t)
-- 
-- instance List Word8 B.ByteString B.ByteString where
--     cons = B.cons
--     nil = B.empty
--     decons bs
--         | B.null bs = Nothing
--         | otherwise = Just (B.head bs, B.tail bs)

mapList f l = case decons l of
    Nothing -> nil
    Just (h,t) -> cons (f h) (mapList f t)

filterList p l = case decons l of
    Nothing -> nil
    Just (h,t)
        | p h       -> cons h (filterList p t)
        | otherwise -> filterList p t

foldrList f z l = case decons l of
    Nothing -> z
    Just (h,t) -> f h (foldrList f z t)

appendList l1 l2 = foldrList cons l2 l1

concatList ls = foldrList appendList nil ls

concatMapList f = foldrList (appendList . f) nil
