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

justWhen p x
    | p x       = Just x
    | otherwise = Nothing

mapMaybe' f = justWhen (not . null) . mapMaybe f

justSigs (Module range name binds decls) =
    fmap (Module range name binds) (mapMaybe' justSigs decls)
justSigs (Record range name mbName binds expr decls) =
    fmap (Record range name mbName binds expr) (mapMaybe' justSigs decls)
justSigs (Mutual    range decls) = 
    fmap (Mutual    range) (mapMaybe' justSigs decls)
justSigs (Abstract  range decls) = 
    fmap (Abstract  range) (mapMaybe' justSigs decls)
justSigs sig@Postulate{} = Just sig
justSigs sig@Primitive{} = Just sig
justSigs sig@Field{}     = Just sig
justSigs sig@TypeSig{}   = Just sig
justSigs x = Nothing
