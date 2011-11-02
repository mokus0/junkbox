module Susie.Module.IRC
    ( ircModule
    ) where

import Susie.Module
import Susie.Module.Console
import Susie.Module.Config

import qualified Susie.Module.IRC.Config as Config

import Control.Monad
import qualified Data.Map as M

ircModule :: SusieModule
ircModule = newModule
    { name = "irc"
    , requires = concatMap provides
        [ consoleModule
        , configModule
        ]
    , run = do
        Config.Servers servers <- getConfigVarWithDefault ["servers"] Config.defaultServers
        connections <- mapM (fmap snd . forkIO . uncurry ircConnection) (M.toList servers)
        mapM_ (>>= either (consoleIO . print) return) connections
    }

ircConnection :: String -> Config.Server -> SusieM ()
ircConnection name conf = do
    putWords ["ircConnection", show name, show conf]
    pause 5

