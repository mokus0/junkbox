#!/usr/bin/env runhaskell
{-# LANGUAGE 
        TypeSynonymInstances, 
        MultiParamTypeClasses, 
        DeriveDataTypeable,
        FlexibleContexts,
        BangPatterns
  #-}
module Main where

import Control.Monad.Event
import Control.Monad.EventM
import Text.PrettyPrint
import Control.Monad.Trans
import Data.Typeable

data Spin = Spin deriving (Typeable)

instance MonadEvent EventM Spin where
    describeEvent Spin = return (text "Spin")
    runEvent Spin = spin

eep = do
--    !eid <- getNextEventId
    liftIO (putChar '.')

grr = getCurrentTime

baz !t = scheduleEventAt (t+1) Spin

spin = do
    eep
    t <- grr
    baz t
    return ()


main = do
    let sim = do
            scheduleEventAt 1000000 StopSim
            doNext Spin

    -- runEventGraph sim
    greebles sim

--runEventGraph_ :: MonadEvent (EventIO Double) e => e -> IO ()
greebles e = do
    state <- newEventIOState (0 :: Double)
    
    runEventIO (scheduleEventIn 0 e) state
    runEventGraphWithState state
