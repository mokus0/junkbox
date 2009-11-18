{-
 -      ``ndiff''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -      
 -      quick & dirty hack to support the following command line:
 -      
 -      > runhaskell pathDups.hs | xargs -L 1 runhaskell ndiff
 -      
 -      which searches the current $PATH for duplicate commands
 -      with different executables
 -}

module Ndiff where

import System.Environment
import System.Directory
import System.Process
import System.Exit

import IO

import Data.List

diff f1 f2 = rawSystem "diff" ["-q", f1, f2]

checkExistence file = do
        isFile <- doesFileExist file
        if isFile
                then return True
                else do
                        isDir <- doesDirectoryExist file
                        hPutStrLn stderr $ if isDir
                                then file ++ ": is a directory"
                                else file ++ ": No such file"
                        return False

main = do
        args <- getArgs
        exist <- mapM checkExistence args
        if and exist
                then do
                        let pairs = [(f1, f2) | (f1:rest) <- tails args, f2 <- rest]
                        mapM_ (uncurry diff) pairs
                else exitWith (ExitFailure 1)