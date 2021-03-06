-- :m Prelude System.Environment Text.Regex Control.Monad System.Posix.Files
-- let path = getEnv "PATH" >>= return.(splitRegex (mkRegex ":"))
-- let which x = path >>= mapM (return.(++ ("/" ++ x))) >>= filterM (fileExist)

module Main where

import System.Environment
import Text.Regex
import Control.Monad
import System.Posix.Files

path = liftM (splitRegex (mkRegex ":")) (getEnv "PATH")
which x = path >>= mapM (return.(++ ("/" ++ x))) >>= filterM (fileExist)

main = 
        getArgs >>= mapM which >>= mapM_ putStrLn . concat