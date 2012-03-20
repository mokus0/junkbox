module FRP.Irc where

import FRP.DiscreteSet
import Reactive.Banana -- 0.5
import qualified Network.SimpleIRC as IRC

data Connection t = Connection 
    { cAddr                 :: String
    , cPort                 :: String
    , cNick                 :: Discrete t String
    , cPass                 :: Maybe String
    , cUsername             :: String
    , cRealname             :: String
    , cChannels             :: DiscreteSet t String
    , cCTCPVersion          :: String
    , cCTCPTime             :: Behavior t String
    , cPingTimeoutInterval  :: Int
    }

connect :: Connection t -> NetworkDescription t (Event t IRC.Message)

