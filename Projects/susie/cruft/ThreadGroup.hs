{-# LANGUAGE DoRec #-}
module Control.Monad.Susie.ThreadGroup
    ( ThreadId, ThreadGroup, new
    , forkIO, forkOS
    , waitForThread
    ) where

import qualified Control.Concurrent as IO
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import qualified Data.Set as S

data ThreadGroup = ThreadGroup !(TVar Int) (TVar (S.Set IO.ThreadId))
data ThreadId a = ThreadId !IO.ThreadId !(MVar (Either SomeException a))

new :: IO ThreadGroup
new = fmap ThreadGroup (newTVarIO S.empty)

forkIO, forkOS :: ThreadGroup -> IO a -> IO (ThreadId a)
forkIO = forkWith IO.forkIO
forkOS = forkWith IO.forkOS

forkWith fork (ThreadGroup tCnt tGrp) action = do
    mVar <- newEmptyMVar
    undefined
    rec tid <- fork (try action >>= putMVar mVar)
    
    return (ThreadId tid mVar)

waitForThread :: ThreadId a -> IO (Either SomeException a)
waitForThread (ThreadId _ mVar) = readMVar mVar