{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}
module Math.Root.Finder where

import Control.Monad.Instances ()

class RootFinder r a b where
    initRootFinder :: (a -> b) -> a -> a -> r a b
    stepRootFinder :: (a -> b) -> r a b -> r a b
    estimateRoot  :: r a b -> a
    estimateError :: r a b -> a
    
    converged :: (Num a, Ord a) => a -> r a b -> Bool
    converged xacc r = abs (estimateError r) <= abs xacc
    
    defaultNSteps :: r a b -> Int
    defaultNSteps _ = 250
    stepsTaken    :: r a b -> Maybe Int
    stepsTaken _ = Nothing

traceRoot :: (Eq (r a b), RootFinder r a b, Num a, Ord a) =>
             (a -> b) -> a -> a -> Maybe a -> [r a b]
traceRoot f a b xacc = go nSteps start (stepRootFinder f start)
    where
        nSteps = defaultNSteps start
        start = initRootFinder f a b
        
        -- lookahead 1; if tracing with no convergence test, apply a
        -- naive test to bail out if the root stops changing.  This is
        -- provided because that's not always the same as convergence to 0,
        -- and the main purpose of this function is to watch what actually
        -- happens inside the root finder.
        go n x next
            | maybe (x==next) (flip converged x) xacc = [x]
            | n <= 0            = []
            | otherwise         = x : go (n-1) next (stepRootFinder f next)

findRoot :: (RootFinder r a b, Num a, Ord a) =>
            (a -> b) -> a -> a -> a -> Either (r a b) (r a b)
findRoot f a b xacc = go nSteps start
    where
        nSteps = defaultNSteps start
        start = initRootFinder f a b
        
        go n x
            | converged xacc x  = Right x
            | n <= 0            = Left  x
            | otherwise         = go (n-1) (stepRootFinder f x)
