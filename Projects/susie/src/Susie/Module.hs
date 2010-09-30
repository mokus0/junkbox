module Susie.Module
    ( Module(..), newModule
    ) where

import Susie.Env

import Data.Dependent.Sum
import Data.Version

data Module m s env = Module
    { name                  :: String
    , version               :: Version
    , unloadable            :: Bool
    , initialize            :: m env
    , cleanup               :: env -> m ()
    , dependencies          :: [Id s]
    , staticExports         :: [DSum (Var s)]
    }

newModule :: Monad m => Module m s env
newModule = Module
    { name                  = error "newModule: no name given"
    , version               = Version [] []
    , unloadable            = False
    , initialize            = return (error "newModule: default environment")
    , cleanup               = \_   -> return ()
    , dependencies          = []
    , staticExports         = []
    }
