module Susie where

import Susie.Main
import Susie.Module.Console
import Susie.Module.Config
import Susie.Module.IRC

main = susie config
    { modules =
        [ configModule
        , consoleModule
        , ircModule
        ]
    }