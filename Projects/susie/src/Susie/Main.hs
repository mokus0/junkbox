module Susie.Main (susie, Config(modules), config) where

import Config.Dyre
import Config.Dyre.Relaunch
import Control.Monad.Susie
import Control.Monad.ST (RealWorld)
import Data.Maybe
import Susie.Module
import Susie.Module.Boot
import System.IO
import System.Exit

data Config = Config
    { configError   :: Maybe String
    , modules   :: [Module SusieM RealWorld]
    }

config = Config
    { configError   = Nothing
    , modules       = []
    }

susie :: Config -> IO ()
susie = wrapMain defaultParams
    { projectName   = "susie"
    , realMain      = realSusie
    , showError     = \cfg err -> cfg { configError = Just err }
    , statusOut     = hPutStrLn stderr
    }

realSusie Config { configError = Just message } = do
    hPutStrLn stderr ("susie: configuration error: " ++ message)
    exitWith (ExitFailure 1)

realSusie config = runSusieM $ do
    setVar bootModules (modules config)
    booted <- loadModule bootModule
    if not booted
        then fail "Failed to load boot module!"
        else do
            runModule       bootModule
            continueWith <- tryReadVar restartWith
            unloadModule    bootModule
            
            if isJust continueWith
                then io (relaunchMaster continueWith)
                else return ()

main = susie config