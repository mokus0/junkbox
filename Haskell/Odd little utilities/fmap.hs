#!/usr/bin/env runhaskell
{-
 -      ``fmap.hs''
 -      (c) 2008 James Cook
 -}

module Main where

import System.Environment
import System.Eval.Haskell
import Control.Monad

main = do
        (func:files) <- getArgs
        (Just f) <- eval ("let f :: String -> String; f = (" ++ func ++ ") in f") [] :: IO (Maybe (String -> String))
        mapM_ (putStrLn . f) files