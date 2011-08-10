{-# LANGUAGE GADTs, DeriveDataTypeable #-}
-- |Configuration interface using a XML Property Lists for a mutable
-- configuration/user-editable-state store.
module Susie.Module.Config
    ( configModule
    , configService
    , getConfigVar
    , getConfigVarWithDefault
    ) where

import Susie.Module
import Susie.Module.Config.Cache
import Data.Maybe
import Data.PropertyList
import Data.Typeable
import Data.GADT.Compare
import Data.GADT.Show
import System.Directory
import System.Environment
import System.FilePath

data State = State
    { confDir       :: !FilePath
    , confCache     :: !ConfigCache
    }

confDirEnvVar = "SUSIE_CONFIG_DIR"

getConfDir :: IO FilePath
getConfDir = do
    envConf <- fmap (lookup confDirEnvVar) getEnvironment
    case envConf of
        Just dir -> return dir
        Nothing -> do
            appDir  <- getAppUserDataDirectory "susie"
            return (appDir </> "config")

initializeState :: IO State
initializeState = do
    dir     <- getConfDir
    cache   <- newCache dir
    return State
        { confDir   = dir
        , confCache = cache
        }

data ConfigVars t where
    ConfigService :: ConfigVars State
    deriving (Typeable)

instance GEq ConfigVars where
    geq ConfigService ConfigService = Just Refl

instance GCompare ConfigVars where
    gcompare ConfigService ConfigService = GEQ

instance GShow ConfigVars where
    gshowsPrec _ ConfigService = showString "ConfigService"

configState   = declare ConfigService
configService = varToId configState

configModule :: SusieModule
configModule = newModule
    { name      = "config"
    , provides  = [configService]
    , onLoad    = io initializeState >>= setVar configState
    
--    , run       = do
--        -- TODO: using a FS change notification service, or polling if necessary,
--        -- check for external changes.  Update "prisine" caches as needed,
--        -- resolving conflicts by comparing timestamps between disk and "dirty" cache.
--        -- TODO: periodically write changes to disk from "dirty" cache
--        return ()
--    
--    , onUnload  = do
--        -- TODO: make sure final config is written
--        return ()
    }

getConfigVar :: PropertyListItem i => [String] -> SusieM (Maybe i)
getConfigVar path = do
    modId <- getCurrentModule
    -- TODO: check "dirty" cache and return value if stored
    -- TODO: read file inte "pristine" cache if necessary
    -- TODO: return value from pristine cache
    
    cache <- fmap confCache (readVar configState)
    plist <- io (prefetch cache modId)
    return (getItemAtKeyPath path (Just plist))

getConfigVarWithDefault :: PropertyListItem i => [String] -> i -> SusieM i
getConfigVarWithDefault path def = fmap (fromMaybe def) (getConfigVar path)

-- setConfigVar :: PropertyListItem i => [String] -> i -> SusieM ()
-- setConfigVar path item = do
--     modName <- fmap moduleName getCurrentModule
--     -- TODO: write changes to a "dirty" cache
--     
--     undefined

