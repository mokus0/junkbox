{-# LANGUAGE RecordWildCards #-}
module Susie.Module
    ( Module(..), newModule
    ) where

import Susie.Module.ModuleID
import Susie.Service.ServiceID

import Control.Monad.Primitive
import Data.Dependent.Sum
import Data.Version

data Module m s = Module
    { name                  :: String
    , version               :: Version
    , initialize            :: ModuleID s -> m ()
    , terminate             :: ModuleID s -> m ()
    
    , services              :: [DSum (ServiceKey s)]
    
    , dependencies          :: [ServiceID s]
    , onServiceDiscovered   :: DSum (ServiceKey s) -> m ()
    , onServiceRevoked      :: ServiceID s -> m ()
    }

newModule :: Monad m => String -> Module m s
newModule name = Module
    { name                  = name
    , version               = Version [] []
    , initialize            = \self -> return ()
    , terminate             = \self -> return ()
    
    , services              = []
    
    , dependencies          = []
    , onServiceDiscovered   = \service -> return ()
    , onServiceRevoked      = \service -> return ()
    }
