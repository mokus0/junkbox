{-# LANGUAGE 
        MultiParamTypeClasses, FunctionalDependencies,
        UndecidableInstances
  #-}
module TypeExperiments.Atomic where

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.ST as ST
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

class Atomic f g | f -> g, g -> f where
    atomically :: f a -> g a

instance Atomic STM.STM IO where atomically = STM.atomically
instance Atomic (ST.ST s) (ST.ST s) where atomically = id
instance Atomic Identity Identity where atomically = id
instance Atomic Maybe Maybe where atomically = id
instance Atomic [] [] where atomically = id
instance Atomic (Reader  r) (Reader  r) where atomically = id
instance Atomic (Writer  w) (Writer  w) where atomically = id
instance Atomic (State   s) (State   s) where atomically = id
instance Atomic (RWS r w s) (RWS r w s) where atomically = id
instance Atomic f g => Atomic (ReaderT  r f) (ReaderT  r g) where atomically (ReaderT x) = ReaderT (atomically . x)
instance Atomic f g => Atomic (WriterT  w f) (WriterT  w g) where atomically (WriterT x) = WriterT (atomically   x)
instance Atomic f g => Atomic (StateT   s f) (StateT   s g) where atomically (StateT  x) = StateT  (atomically . x)
instance Atomic f g => Atomic (RWST r w s f) (RWST r w s g) where atomically (RWST x) = RWST (\r s -> atomically (x r s))
instance Atomic f f => Atomic (ContT    r f) (ContT    r f) where atomically (ContT x) = ContT (\k -> atomically (x (atomically . k)))