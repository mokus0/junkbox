{-# LANGUAGE GADTs, DeriveDataTypeable #-}
-- |This module provides a simple lock mediating access to the console, so that
-- other modules can use it without stepping on one another.
module Susie.Module.Console
    ( consoleModule 
    , console
    , withConsole
    , consoleIO
    , putWords
    ) where

import Susie.Module

import Control.Monad.IO.Peel
import Control.Concurrent.MVar
import Data.Typeable
import Data.GADT.Compare
import Data.GADT.Show

data ConsoleVars t where
    Console :: ConsoleVars (MVar ())
    deriving (Typeable)

instance GEq ConsoleVars where
    geq Console Console = Just Refl

instance GCompare ConsoleVars where
    gcompare Console Console = GEQ

instance GShow ConsoleVars where
    gshowsPrec p Console = showString "Console"

consoleVar = declare Console
console = varToId consoleVar

consoleModule :: SusieModule
consoleModule = newModule
    { name = "console"
    , provides = [console]
    , onLoad = do
        con <- io (newMVar ())
        setVar consoleVar con
    }

withConsole :: SusieM a -> SusieM a
withConsole x = do
    con <- readVar consoleVar
    liftIOOp (withMVar con) (const x)

consoleIO :: IO a -> SusieM a
consoleIO = withConsole . io

putWords = consoleIO . putStrLn . unwords