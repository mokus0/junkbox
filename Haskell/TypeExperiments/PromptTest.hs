{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module TypeExperiments.PromptTest where

import TypeExperiments.PromptProto

import Control.Monad.Prompt
import Control.Monad.State
import Data.IORef

data StatePrompt s a where
    Get :: StatePrompt s s
    Put :: s -> StatePrompt s ()

instance MonadState s (Prompt (StatePrompt s)) where
    get = prompt Get
    put x = prompt (Put x)

startStateHost :: st -> IO (Host (StatePrompt st))
startStateHost (initialState :: st) = do
    state <- newIORef initialState
    
    let handler :: StatePrompt st t -> IO t
        handler Get     = readIORef state
        handler (Put x) = writeIORef state x
    
    startHost handler

helloWorld = do
    put "hello"
    modify (++ " world")
    get

main = do
    host    <- startStateHost ""
    aString <- client host helloWorld
    putStrLn aString
