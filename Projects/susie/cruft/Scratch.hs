{-# LANGUAGE
        GADTs,
        RankNTypes, 
        StandaloneDeriving,
        GeneralizedNewtypeDeriving,
        DeriveDataTypeable,
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts
  #-}
module Scratch where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Primitive
import qualified Data.Dependent.Map as M
import Data.Unique.Prim (Uniq, getUniq)
import Data.GADT.Compare
import Data.Unique.Tag
import Data.Typeable
import Unsafe.Coerce

import Susie.Env

newtype Susie a = Susie {unSusie :: ReaderT [MVar (Env RealWorld)] IO a}
    deriving (Functor, Monad)
instance Applicative Susie where
    pure = return; (<*>) = ap

instance MonadReader (Env RealWorld) Susie where
    ask = Susie $ do
        envMVars <- ask
        envs <- lift (mapM readMVar envMVars)
        return (flattenEnvs envs)
    local f (Susie x) = do
        env <- ask
        Susie $ do
            localMVar <- lift (newMVar (f env))
            local (localMVar:) x

newVar :: String -> Susie (Var RealWorld a)
newVar name = Susie (lift (create name))

-- TODO: search frames for topmost binding and update _that_ binding, if there is one
setVar :: Var RealWorld a -> Entry RealWorld a -> Susie ()
setVar var val = Susie $ do
    envMVar:_ <- ask
    env <- lift (takeMVar envMVar)
    lift (putMVar envMVar (setEnv var val env))

readVar :: Var RealWorld a -> Susie (Maybe (Entry RealWorld a))
readVar var = do
    env <- ask
    return (getEnv var env)

runSusie (Susie x) = do
    env <- newMVar Susie.Env.empty
    runReaderT x [env]
