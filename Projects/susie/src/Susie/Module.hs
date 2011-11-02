{-# LANGUAGE GADTs #-}
-- |Definitions of 'Module' type and related function, and re-export of
-- most of the other types and functions a module implementation would need.
module Susie.Module
    ( module Control.Monad.Susie
    , module Susie.Env.Var
    , module Susie.Env.Var.TH
    
    , SusieModule
    , Module(..)
    , newModule
    
    , ModuleID
    , moduleID
    , moduleName
    , moduleVersion
    ) where

import Control.Monad.Susie
import Control.Monad.ST (RealWorld)
import Susie.ModuleID
import Susie.Env.Var
import Susie.Env.Var.TH

import Data.Version

type SusieModule = Module SusieM RealWorld

data Module m s = Module
    { name                  :: String
    , version               :: Version
    , provides              :: [Id s]
    , requires              :: [Id s]
    , onLoad                :: m ()
    , run                   :: m ()
    , onUnload              :: m ()
    }

newModule :: Monad m => Module m s
newModule = Module
    { name                  = error "newModule: no name given"
    , version               = Version [] []
    , provides              = []
    , requires              = []
    , onLoad                = return ()
    , run                   = return ()
    , onUnload              = return ()
    }

moduleID :: Module m s -> ModuleID
moduleID m = mkModuleID (name m) (version m)
