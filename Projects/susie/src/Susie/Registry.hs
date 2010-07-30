{-# LANGUAGE GADTs, RecordWildCards #-}
module Susie.Registry
    ( Registry
    , insert, lookup
    ) where

import Susie.Module.ModuleID
import Susie.Service.ServiceID

import Prelude hiding (lookup)
import Control.Monad.Primitive
import Data.GADT.Compare
import Data.GADT.Tag (Tag, newTag)
import qualified Data.Dependent.Map as M
import Data.Version

newtype Registry s = Registry (M.DMap (REntryKey s))

data REntry s a = REntry
    { value      :: a
    , providedBy :: ModuleID s
    }

data REntryKey s a where
    REntryKey :: ServiceKey s a -> REntryKey s (REntry s a)

instance GCompare (REntryKey s) where
    gcompare (REntryKey tag1) (REntryKey tag2) = 
        case gcompare tag1 tag2 of
            GLT -> GLT; GEQ -> GEQ; GGT -> GGT

insert :: ServiceKey s a -> ModuleID s -> a -> Registry s -> Registry s
insert key providedBy value (Registry dMap) = Registry (M.insert (REntryKey key) REntry{..} dMap)

lookup :: ServiceKey s a -> Registry s -> Maybe a
lookup key (Registry dMap) = do
    entry <- M.lookup (REntryKey key) dMap
    return (value entry)
    