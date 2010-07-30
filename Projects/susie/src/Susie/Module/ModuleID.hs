module Susie.Module.ModuleID
    ( ModuleID
    , newModuleID
    , moduleUniqueID
    , moduleName
    , moduleVersion
    ) where

import Control.Monad.Primitive
import Data.Unique.Prim (Uniq, getUniq)
import Data.Version

data ModuleID s = ModuleID !(Uniq s) String Version
    deriving (Eq, Ord, Show)

newModuleID :: PrimMonad m => String -> Version -> m (ModuleID (PrimState m))
newModuleID name version = do
    uniq <- getUniq
    return (ModuleID uniq name version)

moduleUniqueID  (ModuleID uid    _    _) = uid
moduleName      (ModuleID   _ name    _) = name
moduleVersion   (ModuleID   _    _ vers) = vers
