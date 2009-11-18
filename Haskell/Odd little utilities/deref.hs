{-
 -      ``deref''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Deref where

import System.Directory
import System.FilePath
import System.Posix.User
import System.Posix.Files

import Control.Exception

isSymLink path = (const (return False)) `handle` do
        status <- getSymbolicLinkStatus path
        return (isSymbolicLink status)

deref ('~':'/':rest) = do
        home <- fmap homeDirectory (getLoginName >>= getUserEntryForName)
        deref (home </> rest)

deref path = do
        isLink <- isSymLink path
        if isLink
                then do
                        link <- readSymbolicLink path
                        if isAbsolute link
                                then deref link
                                else deref (normalise $ takeDirectory path </> link)
                else return path
