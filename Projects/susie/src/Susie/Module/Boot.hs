{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- |The boot module is loaded and unloaded by susie's 'main' function.  Its
-- responsibility is to load, monitor, and unload all configured plugins, 
-- which are provided by 'main' via the 'bootModules' dependency.
module Susie.Module.Boot
    ( bootModule
    , bootModuleName
    , bootModuleVersion
    
    , lastBoot
    , bootModules
    , restartWith
    
    , checkRequirements
    , loadModule
    , runModule
    , unloadModule
    , forceUnloadModule
    ) where

import Control.Concurrent.Thread.Group (wait, nrOfRunning)
import Control.Exception.Peel
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Susie.Internal
import Data.Time
import Susie.Env
import Susie.Module

declareVars [d|
    data ModuleVar t where
        LastBoot    :: ModuleVar UTCTime
        BootModules :: ModuleVar [SusieModule]
        RestartWith :: ModuleVar [String]
    |]

bootModuleName    = name    bootModule
bootModuleVersion = version bootModule

bootModule :: SusieModule
bootModule = newModule
    { name      = "boot"
    , requires  = [varToId bootModules]
    , provides  = [varToId lastBoot]
    
    , onLoad    = do
        now <- io getCurrentTime
        setVar lastBoot now
        return ()
    
    , run       = do
        ms <- readVar bootModules
        withModules ms (runModules ms)
    }


withModules [] = id
withModules (m:ms) 
    = bracket_ (loadModule m) (unloadModule m)
    . withModules ms

runModules ms = do
    tGrp <- getCurrentThreadGroup
    mapM_ (forkIO . runModule) ms
    io (wait tGrp)

checkRequirements :: SusieModule -> SusieM Bool
checkRequirements = allM idIsSet . requires

loadModule :: SusieModule -> SusieM Bool
loadModule m = do
    canRun <- checkRequirements m
    if canRun
        then inModule (moduleID m) $ do
            onLoad m
            loaded <- allM idIsSet (provides m)
            when (not loaded) (forceUnloadModule m)
            return loaded
        
        else do
            fail ("loadModule: requirements not satisfied for module " ++ name m)

runModule :: SusieModule -> SusieM ()
runModule m = inModule (moduleID m) (run m)

unloadModule :: SusieModule -> SusieM Bool
unloadModule m = inModule (moduleID m) $ do
    tGrp <- getCurrentThreadGroup
    nLeft <- stm (nrOfRunning tGrp)
    if nLeft /= 0
        then return False
        else do
            forceUnloadModule m
            return True

forceUnloadModule m = inModule (moduleID m) $ do
    onUnload m
    
    let notMe :: Var s a -> Entry s a -> Bool
        notMe _ entry = provider entry == moduleID m
    modifyEnv_ (filterEnv notMe)
    
    forgetThreadGroup (moduleID m)

