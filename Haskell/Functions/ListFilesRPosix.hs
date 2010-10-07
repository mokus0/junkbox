{-# LANGUAGE ForeignFunctionInterface #-}
module Functions.ListFilesRPosix where

import System.Posix.Directory

import Control.Monad.Trans
import Data.Enumerator hiding (peek)
-- import TypeExperiments.HighLevelIteratees hiding (feed)
-- import TypeExperiments.HighLevelEnumerators
import System.Environment
import System.FilePath

import Foreign
import Foreign.C

newtype DIR = DIR (Ptr DIR)
    deriving (Eq, Show)
newtype DirEnt = DirEnt (Ptr DirEnt)
    deriving (Eq, Show)

foreign import ccall "dirent.h opendir" c_opendir :: CString -> IO DIR
openDir path = withCString path c_opendir

foreign import ccall "dirent.h closedir" closeDir :: DIR -> IO CInt

foreign import ccall "dirent.h readdir" c_readdir :: DIR -> IO DirEnt
readDir dirp = do
    direntp <- c_readdir dirp
    if direntp == DirEnt nullPtr
        then return Nothing
        else return (Just direntp)

foreign import ccall "cbits.h isDir" isDir :: DirEnt -> IO Bool
foreign import ccall "cbits.h getName" c_getName :: DirEnt -> Ptr CString -> Ptr CInt -> IO Bool
getName dirent = alloca $ \name -> 
    alloca $ \nameLen -> do
        True <- c_getName dirent name nameLen
        name <- peek name
        nameLen <- peek nameLen
        peekCStringLen (name, fromIntegral nameLen)


-- enumDir :: FilePath -> Enum2 IOError FilePath IO ()
-- enumDir path = bracket (openDir path) (enumDirStream path) closeDir
-- 
-- enumDirStream :: FilePath -> DIR -> Enum2 IOError FilePath IO ()
-- enumDirStream path dir = do
--     mbDirent <- lift (readDir dir)
--     case mbDirent of
--         Nothing -> return ()
--         Just dirent -> do
--             isd  <- lift (isDir dirent)
--             name <- lift (getName dirent)
--             if isd then enumDir (path </> name)
--                    else do
--                        yield   [path </> name]
--                        enumDirStream path dir
-- 
-- listFiles :: Iter2 FilePath IO ()
-- listFiles = do
--     mbFile <- getSymbol
--     case mbFile of
--         Nothing -> return ()
--         Just file -> do
--             lift (putStrLn file)
--             listFiles
-- 
-- main = do
--     paths <- getArgs
--     sequence_ 
--         [ do 
--             (it, _) <- feed (enumDir path) listFiles
--             runIteratee it
--         | path <- paths
--         ]

-- *sigh*... can we just kill one or the other of mtl / transformers?
-- I _really_ don't care one bit which.
instance MonadTrans (Iteratee e a) where
    lift m = Iteratee $ do
        res <- m
        return (Yield res (Chunks []))

enumDir :: FilePath -> Enumerator e FilePath IO b
enumDir path step = do
    dir <- lift (openDir path)
    
    let feed (Continue k) = do
            mbDirent <- lift (readDir dir)
            case mbDirent of
                Nothing -> returnI (Continue k)
                Just dirent -> do
                    isd  <- lift (isDir dirent)
                    name <- lift (getName dirent)
                    if isd then enumDir (path </> name) (Continue k)
                           else feed ==<< k (Chunks [path </> name])
                    undefined
        feed otherStep = do
            lift (closeDir dir)
            returnI otherStep
    
    feed step

listFiles :: Iteratee e FilePath IO ()
listFiles = do
    mbFile <- Data.Enumerator.head
    case mbFile of
        Nothing -> return ()
        Just file -> do
            lift (putStrLn file)
            listFiles

main = do
    paths <- getArgs
    sequence_ 
        [ runIteratee (enumDir path ==<< listFiles)
        | path <- paths
        ]
