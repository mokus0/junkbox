#!/usr/bin/env runhaskell
module Main where

import Agda.Syntax.Concrete
import Agda.Syntax.Parser
import Agda.Utils.FileName
import Control.Monad
import Data.Maybe
import System.Environment

main = getArgs >>= mapM_ (parseModule >=> dumpSigs . snd)

parseModule = absolute >=> parseFile' moduleParser

dumpSigs = mapM_ print . mapMaybe justSigs

justSigs (Module range name binds decls) = Just (Module range name binds (mapMaybe justSigs decls))
justSigs sig@TypeSig{} = Just sig
justSigs x = Nothing
