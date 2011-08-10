{-# LANGUAGE RecordWildCards #-}
-- |Rather liberal IRC configuration plist format
module Susie.Module.IRC.Config where

import Data.Maybe
import qualified Data.Map as M
import Data.PropertyList

data Server = Server
    { nick      :: String
    , channels  :: [String]
    } deriving (Eq, Ord, Read, Show)

defaultHost = "irc.frobnitz.zonk"
defaultNick = "susie"
defaultChannels = []

defaultServerConfig = Server
    { nick      = defaultNick
    , channels  = defaultChannels
    }

newtype Many a = Many {many :: [a]}
    deriving (Eq, Ord, Read, Show)

newtype Servers = Servers (M.Map String Server)
    deriving (Eq, Ord, Read, Show)

defaultServers = Servers $
    M.singleton defaultHost defaultServerConfig

instance PropertyListItem Server where
    toPropertyList Server{..}
        = fromJust
        . setItemAtKeyPath ["nick"]     (Just nick)
        . setItemAtKeyPath ["channels"] (Just channels)
        $ Nothing
    fromPropertyList plist = Just Server
        { nick      = fromMaybe defaultNick      (getItemAtKeyPath ["nick"]     (Just plist))
        , channels  = maybe defaultChannels many (getItemAtKeyPath ["channels"] (Just plist))
        } 

instance PropertyListItem Servers where
    toPropertyList (Servers ss) = toPropertyList ss
    fromPropertyList = fmap (Servers . either id withDefaults) . fromPropertyList
        where
            withDefaults = M.fromList . map withDefault . many
            withDefault name = (name, defaultServerConfig)

instance PropertyListItem a => PropertyListItem (Many a) where
    toPropertyList = toPropertyList . many
    fromPropertyList = fmap (Many . either id return) . fromPropertyList
