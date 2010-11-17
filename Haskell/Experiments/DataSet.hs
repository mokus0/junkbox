{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}
module Experiments.DataSet where

-- basic idea:
--   A "persistent data set" is an environment in which persistent 
-- references can be managed.  Those references are mutable and persistent,
-- though in this sketch the storage subsystem is not necessarily very 
-- reliable (if the program crashes, it may enter an inconsistent state),
-- nor does it support more than one open instance of a data set.

-- This is a very rough outline of what a simple implementation of such
-- a system _could_ look like.  Obviously there are quite a lot of additional
-- details to be worked out.  Among other things, it really ought to support
-- some kind of garbage collection, which means the "Binary" constraint on
-- objects is not really strong enough.  But the point is more to show what
-- a "functional data store" interface could look like.

-- ideally, a serious implementation would consider:
-- 
--  * robustness in the face of crashes, interrupted operations, etc.
--  * multiple processes accessing and manipulating the same data store.
--    This means non only safely handling the metadata and internal structure 
--    of the store in a multiprocess-friendly way, but also providing
--    useful primitives for the applications to be able to implement safe
--    algorithms for handling the data in the store.  Mutexes or a transaction
--    system come to mind.
--  * one piece of code accessing and manipulating more than one data store
--    at the same time.  If the data store uses a "transactional" model, can
--    one transaction touch multiple stores?
--  * caching, prefetching, etc. - when an object is accessed, it is likely
--    that it and/or its constituents may be accessed again soon.
--  * memory managament - either an explicit delete for every object, named 
--    or not, or (much more preferably) a garbage collector.  GC adds 
--    significant additional complexity to the multi-process problem.

import Control.Applicative
import Control.Exception
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Data.Dependent.Map (DMap, DSum(..))
import qualified Data.Dependent.Map as M
import Data.GADT.Tag
import Data.IORef
import Data.Typeable
import System.FilePath

newtype PersistentT m t = PersistentT (ReaderT (DataStore (PrimState m)) m t)
    deriving (Functor, Applicative, Monad, MonadIO)

data DataStore s = DataStore
    { dsRoot :: FilePath
    , dsOpenObjects :: IORef (DMap (OpenObjectID s))
    , dsRootObject :: Tag s RootObject
    }

newtype Object s t = Object (Tag s t)
instance Binary (Object s t)

data OpenObjectID s t where
    OpenObjectID :: Tag s t -> OpenObjectID s (OpenObject t)

instance GCompare (OpenObjectID s) where
    gcompare (OpenObjectID x) (OpenObjectID y) = 
        case gcompare x y of GLT -> GLT; GEQ -> GEQ; GGT -> GGT

data OpenObject t where 
    OpenObject :: Binary t => FilePath -> t -> OpenObject t

data RootObject = RootObject
    { nextObjectId  :: Integer
    }

newRootObject = RootObject 0

instance Binary RootObject where
    put obj = do
        put (nextObjectId obj)
    get = do
        nextOID <- get
        return (RootObject nextOID)

openDataStore :: FilePath -> IO (DataStore RealWorld)
openDataStore root = do
    rootObjectTag <- newTag :: IO (Tag RealWorld RootObject)
    rootObject <- openDataObject rootObjectTag (root </> "root")
    
    openObjects <- newIORef (M.singleton (OpenObjectID rootObjectTag) rootObject)
    
    return DataStore
        { dsRoot = root
        , dsOpenObjects = openObjects
        , dsRootObject = rootObjectTag
        }

openDataObject :: Binary t => Tag s t -> FilePath -> IO (OpenObject t)
openDataObject tag path = do
    object <- fmap decode (BL.readFile path)
    return (OpenObject path object)

closeDataStore :: DataStore RealWorld -> IO ()
closeDataStore ds = do
    openObjs <- readIORef (dsOpenObjects ds)
    mapM_ closeDataObject (M.toList openObjs)

closeDataObject :: DSum (OpenObjectID RealWorld) -> IO ()
closeDataObject (OpenObjectID tag :=> OpenObject path obj) = do
    BL.writeFile path (encode obj)

withDataStore :: FilePath -> PersistentT IO t -> IO t
withDataStore root (PersistentT action) = bracket
    (openDataStore root)
    closeDataStore
    (runReaderT action)

newDataStore :: FilePath -> IO ()
newDataStore root = do
    undefined

getNextObjectID :: PersistentT IO Integer
getNextObjectID = PersistentT $ do
    rootTag <- asks dsRootObject
    modifyIORef

newObject :: Binary t => t -> PersistentT IO (Object RealWorld t) 
newObject initVal = do
    undefined

-- |Create a new "named" object.  If this system had a garbage collector,
-- the "named" objects would be the "root" objects of the reference graph.
newNamedObject :: Binary t => String -> t -> PersistentT IO (Object RealWorld t)
newNamedObject = undefined

forgetNamedObject :: String -> PersistentT IO ()
forgetNamedObject = undefined
