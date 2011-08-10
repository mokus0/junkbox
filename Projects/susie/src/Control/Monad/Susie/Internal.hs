{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Susie.Internal where

import Control.Applicative
import Control.Concurrent.Thread.Group as ThreadGroup
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Peel
import Control.Monad.Reader
import Control.Monad.ST (RealWorld)
import qualified Data.Map as M
import Susie.Env
import Susie.ModuleID
import {-# SOURCE #-} Susie.Module.Boot

data SusieState = SusieState
    { currentModule :: !ModuleID
    , threadGroups  :: !(MVar (M.Map ModuleID ThreadGroup))
    , environment   :: !(TVar (Env RealWorld))
    }

newtype SusieM t = SusieM { unSusieM :: ReaderT SusieState IO t }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPeelIO)

initSusieState = do
    env     <- newTVarIO Susie.Env.empty
    tGrps   <- newMVar M.empty
    return SusieState
        { currentModule = mkModuleID bootModuleName bootModuleVersion
        , threadGroups  = tGrps
        , environment   = env
        }

runSusieM :: SusieM t -> IO t
runSusieM (SusieM x) = do
    state <- initSusieState
    runReaderT x state

getCurrentModule = SusieM (asks currentModule)
inModule m (SusieM x) = SusieM (local (\s -> s {currentModule = m}) x)

modifyEnv f = SusieM $ do
    e <- asks environment
    liftIO $ atomically $ do
        x <- readTVar e
        let (y,z) = f x
        writeTVar e y
        return z

modifyEnv_ f = SusieM $ do
    e <- asks environment
    liftIO . atomically $ do
        x <- readTVar e
        writeTVar e (f x)

getThreadGroup modId = SusieM $ do
    tGrps  <- asks threadGroups
    liftIO $ modifyMVar tGrps $ \grps -> do
            case M.lookup modId grps of
                Just modGrp     -> return (grps, modGrp)
                Nothing         -> do
                    modGrp <- ThreadGroup.new
                    return (M.insert modId modGrp grps, modGrp)

tryGetThreadGroup modId = SusieM $ do
    tGrps  <- asks threadGroups
    grps <- liftIO (readMVar tGrps)
    return (M.lookup modId grps)

forgetThreadGroup modId = SusieM $ do
    tGrps <- asks threadGroups
    liftIO (modifyMVar_ tGrps (return . M.delete modId))

getCurrentThreadGroup = do
    modId <- SusieM (asks currentModule)
    getThreadGroup modId
    