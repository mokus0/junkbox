{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module TypeExperiments.STT 
    ( ST, runST
    , STT, runSTT
    , STRef, newSTRef, readSTRef, writeSTRef
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Data.Dependent.Map as M
import Data.Unique.Tag

import Unsafe.Unique.Tag

type ST s = STT s Identity

runST :: (forall s. ST s a) -> a
runST = runIdentity . runSTT

newtype STT s m a = STT { unSTT :: StateT (Integer, DMap (Tag s)) m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadTrans)

runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT it = evalStateT (unSTT it) (0, M.empty)

newtype STRef s a = STRef (Tag s a)

-- I'm somewhat surprised that this type isn't inferred correctly.
-- with all the let bindings inferred monomorphically, shouldn't it be well-enough
-- constrained?
-- Actually, it's inferred correctly with -XGADTs but not without!  Very odd, 
-- especially considering Tag is not even implemented using GADTs...
newSTRef :: Monad m => a -> STT s m (STRef s a)
newSTRef x = STT $ do
    (i, m) <- get
    let k  = veryUnsafeMkTag i
        i' = i+1
        m' = M.insert k x m
    i' `seq` m' `seq` put (i', m')
    return (STRef k)

readSTRef (STRef k) = STT $ do
    (_,m) <- get
    return (m M.! k)

writeSTRef (STRef k) v = STT $ do
    (i,m) <- get
    let m' = M.insert k v m
    m' `seq` put (i,m')

