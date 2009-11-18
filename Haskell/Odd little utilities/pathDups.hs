{-
 -      ``pathDups''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -      
 -      quick & dirty hack to support the following command line:
 -      
 -      > runhaskell pathDups.hs | xargs -L 1 runhaskell ndiff
 -      
 -      which searches the current $PATH for duplicate commands
 -      with different executables
 -}

module PathDups where

import System.Environment
import System.Directory
import System.FilePath
import System.Info
import System.Posix.Files

import Control.Monad
import Control.Monad.Instances
import Data.List

import qualified Data.Map as M

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p str = mapTail tail (groupBy (const (not.p)) str)
        where mapTail f = liftM2 (:) head (map f . tail)

getExecutableSearchPath :: IO [FilePath]
getExecutableSearchPath = do
        let sep = if os == "mingw32"
                then ';'
                else ':'
        pathEnv <- getEnv "PATH"
        return (splitOn (== sep) pathEnv)

isExecutable :: FilePath -> IO Bool
isExecutable path = do
        exists <- doesFileExist path
        if exists
                then fmap executable (getPermissions path)
                else return False

getExecutablesInDir :: FilePath -> IO [FilePath]
getExecutablesInDir dir = do
        files <- fmap (filter (not . flip elem [".", ".."])) (getDirectoryContents dir)
        let fullPaths = map (dir </>) files
        filterM isExecutable fullPaths

output :: (String, [String]) -> IO ()
output (cmd, locs) = putStrLn (unwords locs)

main :: IO ()
main = do
        path <- getExecutableSearchPath
        files <- mapM getExecutablesInDir path
        
        let filesByName = M.fromListWith (flip (++)) (map index (concat files))
                where index file = (takeFileName file, [file])
        let duplicates = M.filter ((>2).length) filesByName
        
        mapM_ output (M.toList duplicates)