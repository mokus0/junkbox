{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}
module NR.Ch9.S1 where

import Control.Monad.Instances

-- |Given a function and an initial guessed range x1 to x2, this function
-- expands the range geometrically until a root is bracketed by the returned
-- values, returning a list of the successively expanded ranges.  The
-- list will be finite if and only if the sequence yields a bracketing pair.
zbrac f x1 x2
    | x1 == x2  = error "zbrac: empty range"
    | otherwise = go x1 (f x1) x2 (f x2)
    where
        ntry = 50
        factor = 1.6
        go x1 f1 x2 f2
            | signum f1 /= signum f2    = [(x1, x2)]
            | abs f1 < abs f2           = (x1, x2) : go x1' (f x1') x2 f2
            | otherwise                 = (x1, x2) : go x1 f1 x2' (f x2')
            where 
                x1' = x1 - factor * w
                x2' = x2 + factor * w
                w = x2 - x1

-- |Given a function defined on the interval [x1,x2], subdivide the interval
-- into n equally spaced segments and search for zero crossings of the function.
-- The returned list will contain all bracketing pairs found.
zbrak f x1 x2 n = 
    [ (x1, x2)
    | ((x1, y1), (x2, y2)) <- zip xys (tail xys)
    , signum y1 /= signum y2
    ]
    where
        dx = (x2 - x1) / fromIntegral n
        xs = x1 : [x1 + dx * fromIntegral i | i <- [1..n]]
        xys = map (\x -> (x, f x)) xs

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

findRoot f a b xacc = go nSteps start
    where
        nSteps = defaultNSteps start
        start = initRootFinder f a b
        
        go n x
            | converged xacc x  = Right x
            | n <= 0            = Left  x
            | otherwise         = go (n-1) (stepRootFinder f x)

-- |Bisect an interval in search of a root.  At all times, @f (estimateRoot _)@
-- is less than or equal to 0 and @f (estimateRoot _ + estimateError _)@ is 
-- greater than or equal to 0.
data Bisect a b = Bisect { bisX :: !a, bisF :: !b , bisDX :: !a }
    deriving (Eq, Ord, Show)

instance (Fractional a, Ord b, Num b) => RootFinder Bisect a b where
    initRootFinder f x1 x2
        | f1 < 0    = Bisect x1 f1 (x2-x1)
        | otherwise = Bisect x2 f2 (x1-x2)
        where f1 = f x1; f2 = f x2
    
    stepRootFinder f orig@(Bisect x fx dx) = case fMid `compare` 0 of
            LT ->  Bisect xMid fMid dx2
            EQ ->  orig
            GT ->  Bisect x fx dx2 
            where
                dx2 = dx * 0.5
                xMid = x + dx2
                fMid = f xMid
    
    estimateRoot  (Bisect x _  _) = x
    
    estimateError (Bisect _ 0  _) = 0
    estimateError (Bisect _ _ dx) = dx

-- |Using bisection, return a root of a function known to lie between x1 and x2.
-- The root will be refined till its accuracy is +-xacc.
rtbis :: (Ord a, Fractional a, Ord b, Num b) => (a -> b) -> a -> a -> a -> Either (Bisect a b) a
rtbis f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)
