module Control.Monad.Susie
    ( SusieM, runSusieM, io, stm
    , getCurrentModule
    , setVar
    , readVar, tryReadVar
    , varIsSet, idIsSet
    , forkIO, forkOS, ThreadId, Result(..)
    , Exception(..), SomeException(..), throw
    , pause
    ) where

import Control.Concurrent (ThreadId, threadDelay)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.Thread (Result)
import qualified Control.Concurrent.Thread.Group as ThreadGroup
import Control.Exception (Exception(..), SomeException(..), throwIO)
import Control.Monad.Reader
import Control.Monad.Susie.Internal
import Control.Monad.ST (RealWorld)
import Data.Maybe
import Susie.Env

io :: IO t -> SusieM t
io = liftIO

stm :: STM t -> SusieM t
stm = io . atomically

setVar :: Var RealWorld t -> t -> SusieM ()
setVar v x = do
    m <- SusieM (asks currentModule)
    modifyEnv_ (setEnv v (Entry m x))

readVar :: Var RealWorld t -> SusieM t
readVar v = do
    mbX <- tryReadVar v
    case mbX of
        Nothing -> fail "readVar: variable is not set"
        Just x  -> return x

tryReadVar :: Var RealWorld t -> SusieM (Maybe t)
tryReadVar v = modifyEnv (\e -> (e, fmap value (getEnv v e)))

varIsSet :: Var RealWorld t -> SusieM Bool
varIsSet v = do
    mbX <- tryReadVar v
    return (isJust mbX)

idIsSet :: Id RealWorld -> SusieM Bool
idIsSet v = idToVar v varIsSet

forkIO :: SusieM a -> SusieM (ThreadId, SusieM (Result a))
forkIO = forkBy ThreadGroup.forkIO

forkOS :: SusieM a -> SusieM (ThreadId, SusieM (Result a))
forkOS = forkBy ThreadGroup.forkOS

forkBy fork (SusieM (ReaderT x)) = do
    tGrp <- getCurrentThreadGroup
    SusieM . ReaderT $ \r -> do
        (tid, result) <- fork tGrp (x r)
        return (tid, io result)

throw :: Exception e => e -> SusieM t
throw = io . throwIO

pause :: Double -> SusieM ()
pause seconds = io (threadDelay (round (seconds * 1e6)))
