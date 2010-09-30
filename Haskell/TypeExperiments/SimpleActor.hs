{-# LANGUAGE RankNTypes, ExistentialQuantification, RecordWildCards #-}
module TypeExperiments.SimpleActor where

import Control.Concurrent
import Control.Monad.LoopWhile
import Control.Monad.Trans
import Data.IORef
import TypeExperiments.MsgChan

data Actor f m = forall st. Actor
    { name          :: !String
    , initialize    :: m st
    , receive       :: forall a. f a -> st -> m (a, st)
    }

actor :: Monad m => Actor f m
actor = Actor
    { name          = error "actor: no name specified"
    , initialize    = return ()
    , receive       = error "actor: no receive operation" 
    }

newtype OrDone f a where
    Msg :: f a -> OrDone f a
    Done :: OrDone f ()

runActor Actor{..} = do
    state <- initialize
    state <- newIORef state
    msgQueue <- newChan
    forkIO $ loop $ do
        msg <- lift (readChan msgQueue)
        case msg of
            Done -> while False
            Just (Rq req respVar) -> lift $ do
                st <- readIORef state
                (resp, st) <- receive req st
                writeIORef state st
                putMVar respVar resp
    
    return msgQueue
        

