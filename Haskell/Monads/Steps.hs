{-# LANGUAGE ParallelListComp #-}
module Monads.Steps where

import Control.Monad.Writer
import Text.Printf

newtype Steps m a = Steps {unSteps :: WriterT [(String, m ())] m a}

instance Monad m => Functor (Steps m) where fmap f (Steps x) = Steps (fmap f x)
instance Monad m => Monad   (Steps m) where
    fail    = Steps . fail
    return  = Steps . return
    Steps x >>= f = Steps (x >>= unSteps.f)

step desc action = Steps (tell [(desc, action)])

runSteps (Steps ss) eval = do
    (a,steps) <- runWriterT ss
    b <- eval steps
    return (a,b)

progress disp steps = sequence_
    [ do
        disp i n desc
        step
    | (desc, step) <- steps
    | i <- [1..] :: [Int]
    ] 
    where n = length steps

simpleProgress :: [(String, IO ())] -> IO ()
simpleProgress = progress (printf "Executing step %d of %d: %s\n")