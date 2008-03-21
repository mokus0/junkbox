{-
 -      ``Data/ReOrdMap.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Data.ReOrdMap where

import Util.ReOrd (ReOrd(..))
import qualified Data.Map as M

data ReOrdMap k v = ReOrdMap (k -> k -> Ordering) (M.Map (ReOrd k) v)

empty :: (k -> k -> Ordering) -> ReOrdMap k v
empty f = ReOrdMap f M.empty

(!) :: ReOrdMap k v -> k -> v
(ReOrdMap f m) ! k = m M.! (ReOrd f k)

lookup :: (Monad m) => k -> ReOrdMap k v -> m v
lookup k (ReOrdMap f m) = M.lookup (ReOrd f k) m

minView :: (Monad m) => ReOrdMap k v -> m (v, ReOrdMap k v)
minView (ReOrdMap f m) = do
        (v, rest) <- M.minView m
        return (v, ReOrdMap f rest)

maxView :: (Monad m) => ReOrdMap k v -> m (v, ReOrdMap k v)
maxView (ReOrdMap f m) = do
        (v, rest) <- M.maxView m
        return (v, ReOrdMap f rest)

minViewWithKey :: (Monad m) => ReOrdMap k v -> m ((k, v), ReOrdMap k v)
minViewWithKey (ReOrdMap f m) = do
        ((ReOrd _ k, v), rest) <- M.minViewWithKey m
        return ((k, v), ReOrdMap f rest)

maxViewWithKey :: (Monad m) => ReOrdMap k v -> m ((k, v), ReOrdMap k v)
maxViewWithKey (ReOrdMap f m) = do
        ((ReOrd _ k, v), rest) <- M.maxViewWithKey m
        return ((k, v), ReOrdMap f rest)

