{-# LANGUAGE EmptyDataDecls, GADTs, TypeFamilies, TypeOperators, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Experiments.DerivedRefs where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.Loops (allM)
import Data.List (nub)

data T
data F

type family   And a b
type instance And T t = t
type instance And F t = F

type family   Or a b
type instance Or T t = T
type instance Or F t = t

newtype Gen = Gen Int deriving (Enum, Eq)
data Dep where
    Dep :: Ref r w a -> TVar Gen -> Dep
data Ref r w a where
    Ref         :: !(TVar (Gen, a)) -> Ref r w a
    Derived     :: [Dep] -> STM a -> !(TVar (Gen, a)) -> Ref r F a

instance Eq (Ref r w a) where
    Ref r1 == Ref r2    = r1 == r2
    Derived _ _ r1 == Derived _ _ r2
                        = r1 == r2      -- true by construction
    _ == _              = False
instance Eq Dep where
    Dep _ g1 == Dep _ g2    = g1 == g2  -- true by construction

newRefIO :: t -> IO (Ref r w t)
newRefIO x = fmap Ref (newTVarIO (Gen 0, x))

newRef :: t -> STM (Ref r w t)
newRef x = fmap Ref (newTVar (Gen 0, x))

updateRef :: Ref r w a -> STM ()
updateRef r@(Ref _) = return ()
updateRef (Derived deps update cache) = do
    upToDate <- allM checkDep deps
    when (not upToDate) $ do
        newVal <- update
        oldGen <- fmap fst (readTVar cache)
        let newGen = succ oldGen
        writeTVar cache (newGen, newVal)
        mapM_ updateDep deps

updateDep (Dep ref genRef) = do
    gen <- getGen ref
    writeTVar genRef gen

getGen :: Ref r w a -> STM Gen
getGen (Ref r) = fmap fst (readTVar r)
getGen r@(Derived _ _ cache) = do
    updateRef r
    fmap fst (readTVar cache)

checkDep (Dep ref genRef) = do
    depGen <- readTVar genRef
    refGen <- getGen ref
    return (depGen == refGen)

flattenDep :: Dep -> [Dep]
flattenDep (Dep (Derived deps _ _) _) = deps >>= flattenDep
flattenDep d = return d

readRef :: Ref T w t -> STM t
readRef (Ref r) = fmap snd (readTVar r)
readRef r@(Derived _ _ cache) = do
    updateRef r
    fmap snd (readTVar cache)

newDep :: Ref r w a -> STM Dep
newDep r = do
    depGen <- getGen r
    depGenRef <- newTVar depGen
    return (Dep r depGenRef)

mkDerived :: [Dep] -> STM a -> STM (Ref r F a)
mkDerived deps update = do
    val <- update
    cache <- newTVar (Gen 0, val)
    return (Derived (nub (deps >>= flattenDep)) update cache)

refMap :: (a -> b) -> Ref T w a -> STM (Ref r F b)
refMap f r = do
    dep <- newDep r
    mkDerived [dep] (fmap f (readRef r))

refZipWith :: (a -> b -> c) -> Ref T w1 a -> Ref T w2 b -> STM (Ref r F c)
refZipWith f r1 r2 = do
    dep1 <- newDep r1
    dep2 <- newDep r2
    mkDerived [dep1, dep2] $ do
        v1 <- readRef r1
        v2 <- readRef r2
        return (f v1 v2)

writeRef :: Ref r T a -> a -> STM ()
writeRef (Ref r) x = do
    (gen, _) <- readTVar r
    writeTVar r (succ gen, x)
