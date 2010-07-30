{-# LANGUAGE GADTs #-}
module Susie.Service.ServiceID
    ( ServiceKey, ServiceID
    , newServiceKey
    , serviceName, serviceKeyID
    ) where

import Control.Monad.Primitive
import Data.Unique.Prim (Uniq, getUniq)
import Data.GADT.Compare
import Data.GADT.Tag

data ServiceID s where
    ServiceID :: ServiceKey s a -> ServiceID s

data ServiceKey s a = ServiceKey !(Tag s a) String deriving (Eq, Ord)

newServiceKey :: PrimMonad m => String -> m (ServiceKey (PrimState m) a)
newServiceKey name = do
    tag <- newTag
    return (ServiceKey tag name)

serviceKeyID key = ServiceKey key
serviceName      (ServiceKey   _ name) = name

instance GCompare (ServiceKey s) where
    gcompare (ServiceKey k1 _) (ServiceKey k2 _) = gcompare k1 k2

