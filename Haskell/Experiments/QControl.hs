{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Experiments.QControl where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.State
import Data.IORef
import Language.Haskell.TH

-- the goal:
liftQOp :: forall a b t. MonadTransControl t => (Q a -> Q b) -> t Q a -> t Q b
liftQOp = undefined

-- or maybe:
liftQOp' :: MonadBaseControl Q m => (Q a -> Q b) -> m a -> m b
liftQOp' = undefined

-- or at least:
liftQOpState :: forall s a b. (Q a -> Q b) -> StateT s Q a -> StateT s Q b
liftQOpState qOp stX = do
    state <- lift . runIO . newIORef =<< get
    y <- lift . qOp $ do
        (x, newSt) <- runStateT stX =<< runIO (readIORef state)
        runIO (writeIORef state newSt)
        return x
    lift (runIO (readIORef state)) >>= put
    return y

-- so that we can do things like:
example :: StateT s Q Exp -> StateT s Q [Dec]
example = liftQOpState $ \expQ -> 
    [d| 
        foo = $expQ
        bar = $expQ
     |]

counter :: StateT Int Q Exp
counter = do
    i <- get
    put $! i+1
    lift [| i |]
    