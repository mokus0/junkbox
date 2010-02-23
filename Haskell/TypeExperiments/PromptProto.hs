{-# LANGUAGE GADTs, RankNTypes #-}
module TypeExperiments.PromptProto (Host, startHost, quitHost, client) where

import Control.Concurrent
import Control.Monad.LoopWhile
import Control.Monad.Prompt
import Control.Monad.Trans

-- using the Prompt monad to implement ad-hoc channel protocols
data Request p where
    Quit    :: Request p
    Request :: !(p t) -> MVar t -> Request p

newtype Host p = Host (Chan (Request p))

startHost :: (forall t. p t -> IO t) -> IO (Host p)
startHost handler = do
    chan <- newChan
    forkIO (host handler chan)
    return (Host chan)

host :: (forall t. p t -> IO t) -> Chan (Request p) -> IO ()
host handler chan = loop hostloop
    where
        hostloop :: LoopWhileT IO ()
        hostloop = do
            rq <- lift (readChan chan)
            case rq of
                Quit            -> while False
                Request rq resp -> lift $ do
                    ans <- handler rq
                    putMVar resp ans

quitHost :: Host p -> IO ()
quitHost (Host chan) = writeChan chan Quit

client :: Host p -> Prompt p t -> IO t
client (Host chan) = runPromptM clientRequest
    where
        clientRequest rq = do
            resp <- newEmptyMVar
            writeChan chan (Request rq resp)
            takeMVar resp

