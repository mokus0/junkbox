{-# LANGUAGE 
        GADTs,
        GeneralizedNewtypeDeriving,
        KindSignatures
  #-}
module Experiments.Sensorium where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Operational

data Stream s
    = Chunks [s]
    | EOF
    deriving (Eq, Show)
data React s a where
    GetSym :: React s (Stream s)
newtype Reactor s m a = Reactor { unReactor :: ProgramT (React s) (StateT (Stream s) m) a }
    deriving (Functor, Monad)
instance Monad m => Applicative (Reactor s m) where
    pure = return
    (<*>) = ap

-- The desired interface (rough sketch):
data Sensorium s (m :: * -> *) = Sensorium

empty :: Sensorium s m
empty = Sensorium

addReactor :: Reactor s m a -> (a -> m ()) -> Sensorium s m -> Sensorium s m
addReactor = undefined

addStimulus :: s -> Sensorium s m -> m (Sensorium s m)
addStimulus = undefined
