#!/usr/bin/env runhaskell
{-
 -      ``FunWithHint''
 -}

module Main where

import Language.Haskell.Interpreter.GHC
import Language.Haskell.Interpreter.GHC.Unsafe

main = do
    s <- newSession
    hashes <- withSession s $ do
        unsafeSetGhcOption "-XTemplateHaskell"
        setImports ["Prelude", "Language.Haskell.TH"]
        
        -- using template haskell to refer to our own "unpronounceable" let-binding
        interpret "'#' : $( varE (mkName (\"foo_\" ++ [succ $ succ '/'])) )" (infer)
    
    putStrLn (take 72 hashes)
    
    action <- withSession s $ do
        interpret "putStrLn \"Haskell-within-a-haskell is trying to do IO!\"" (as :: IO ())
    
    putStrLn "got an IO () thingy:  we don't have to, but I'm gonna run it:"
    action
    
    action <- withSession s $ do
        setImports ["Prelude"]
        
        interpret "$( Language.Haskell.TH.runIO (putStrLn \"illegally doing IO with template haskell, even though runIO isn't in scope!\") >> [| putStrLn \"doing IO the nice way\" |] )" (as :: IO ())
    
    putStrLn "got an IO () thingy:  not running it, but it's kinda too late!"