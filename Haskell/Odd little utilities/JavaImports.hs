#!/usr/bin/env runhaskell
module Main where

import Control.Monad
import qualified Data.Set as S
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax
import System.Environment

javaImports = either (error . show) (S.fromList . extract) . parseCompilationUnit
    where extract (CompilationUnit _ imports _) = map (show . pretty) imports

main = do
    files <- getArgs
    print files
