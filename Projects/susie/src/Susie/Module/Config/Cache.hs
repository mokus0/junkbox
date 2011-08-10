module Susie.Module.Config.Cache where

import Susie.Module
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.PropertyList
import System.Directory
import System.FilePath

data ConfigCache = Cache
    { locate    :: ModuleID -> IO FilePath
    , fetch     :: FilePath -> IO PropertyList
    , cacheData :: !(MVar (M.Map ModuleID PropertyList))
    }

defaultLocate confDir modId = do
    return (confDir </> moduleName modId <.> "plist")

defaultFetch path = do
    exists <- doesFileExist path
    if exists
        then readPropertyListFromFile path
        else return (plDict (M.fromList []))

newCache confDir = do
    mVar <- newMVar M.empty
    return Cache
        { locate    = defaultLocate confDir
        , fetch     = defaultFetch
        , cacheData = mVar
        }

prefetch cache modId = do
    modifyMVar (cacheData cache) $ \c -> do
        case M.lookup modId c of
            Just p  -> return (c, p)
            Nothing -> do
                p <- locate cache modId >>= fetch cache
                
                return (M.insert modId p c, p)
