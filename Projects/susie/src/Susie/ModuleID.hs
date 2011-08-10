module Susie.ModuleID
    ( ModuleID
    , mkModuleID
    , moduleName
    , moduleVersion
    ) where

import Data.Version

data ModuleID = ModuleID !String !Version
    deriving (Eq, Ord, Read, Show)

mkModuleID :: String -> Version -> ModuleID
mkModuleID name version = 
    ModuleID name version

moduleName      (ModuleID name    _) = name
moduleVersion   (ModuleID    _ vers) = vers
