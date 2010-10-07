module Functions.ListFilesR where

import Prelude  -- hiding (catch, concat)
-- import Data.DList hiding (map)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.FilePath
import System.Environment
import System.IO.Unsafe
import TypeExperiments.HighLevelIteratees hiding (feed)
import TypeExperiments.HighLevelEnumerators

-- listFilesR :: FilePath -> IO [FilePath]
-- listFilesR path = go empty [path]
--     where
--         go files []         = return (toList files)
--         go files (dir:dirs) = do
--             allFiles <- getProperDirectoryContents dir
--             (subDirs, dirFiles) <- partitionM doesDirectoryExist allFiles
--             
--             go (append files dirFiles) (toList subDirs ++ dirs)
-- 
getProperDirectoryContents dir 
    = map (dir </>) . filter isDODD
    <$> getDirectoryContents dir
    where
        isDODD "." = False
        isDODD ".." = False
        isDODD _ = True

-- partitionM p = go empty empty
--     where
--         go yes no [] = return (yes, no)
--         go yes no (x:xs) = do
--             test <- p x
--             if test
--                 then go (yes `snoc` x) no xs
--                 else go yes (no `snoc` x) xs
-- 

-- listFilesR :: FilePath -> IO [FilePath]
-- listFilesR path = 
--   let
--     isDODD "." = False
--     isDODD ".." = False
--     isDODD _ = True
--   in unsafeInterleaveIO $ do
--     allfiles <- getDirectoryContents path
--     dirs <- forM allfiles $ \d ->
--       if isDODD d then
--         do let p = path </> d
--            isDir <- doesDirectoryExist p
--            if isDir then listFilesR p else return [d]
--         else return []
--     return $ concat dirs
-- 
-- main = do
--     paths <- getArgs
--     files <- mapM listFilesR paths
--     print (sum (map length files))


enumDir :: FilePath -> Enum2 IOError FilePath IO ()
enumDir dir = do
    contents <- lift (getProperDirectoryContents dir)
    sequence_
        [ do
            isDir <- lift (doesDirectoryExist item)
            if isDir
                then enumDir item
                else yield [item]
        | item <- contents
        ]

listFiles :: Iter2 FilePath IO ()
listFiles = do
    mbFile <- getSymbol
    case mbFile of
        Nothing -> return ()
        Just file -> do
            lift (putStrLn file)
            listFiles

main = do
    paths <- getArgs
    sequence_ 
        [ do 
            (it, _) <- feed (enumDir path) listFiles
            runIteratee it
        | path <- paths
        ]
