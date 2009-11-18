{-
 -      ``lsTree''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module LsTree where

import System.Directory
import System.Environment
import System.FilePath

import Control.Monad

main = do
        roots <- getArgs
        mapM_ (printTree "") roots

getProperDirectoryContents = fmap (filter (not . flip elem [".", ".."])) . getDirectoryContents

printTree prefix dir = do
        isDir <- doesDirectoryExist dir
        contents <- if isDir
                then getProperDirectoryContents dir
                else return []
        let qualified = map (dir </>) contents
        putStrLn (prefix ++ takeFileName (dropTrailingPathSeparator dir))
        mapM_ (printTree ('\t':prefix)) qualified