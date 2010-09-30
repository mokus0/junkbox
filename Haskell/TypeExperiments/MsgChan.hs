{-# LANGUAGE GADTs #-}
module TypeExperiments.MsgChan where

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data Rq f where
    Rq :: f a -> MVar a -> Rq f
newtype MsgChan f = MsgChan (Chan (Rq f))

syncRq c f = join (asyncRq c f)

asyncRq (MsgChan c) f = do
    res <- newEmptyMVar
    writeChan c (Rq f res)
    return (takeMVar res)

