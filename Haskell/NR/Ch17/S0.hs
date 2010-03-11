{-# LANGUAGE RecordWildCards #-}
module NR.Ch17.S0 
    ( Derivs, Stepper, Step(..)
    , sparse
    
    , IntegrationSettings(..)
    , defaultIntegrationSettings
    , integrate
    ) where

import Control.Monad
import Control.Monad.Writer
import Data.Ord

type Derivs a = a -> [a] -> [a]
type Stepper m a = Derivs a -> a -> [a] -> [a] -> a -> m ([a], a, Step a [a])

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y = case cmp x y of GT -> y; _ -> x

data IntegrationSettings a = IntegrationSettings
    { maxStp    :: Int
    , hMin      :: a
    }

defaultIntegrationSettings :: Num a => IntegrationSettings a
defaultIntegrationSettings = IntegrationSettings
    { maxStp    = 50000
    , hMin      = 0
    }

data Step a b = Step
    { from  :: (a, b)
    , to    :: (a, b)
    , hUsed :: !a
    , dense :: a -> b
    }

sparse Step{..} = to

-- generic integration driver
integrate :: (Monad m, Num a, Ord a) =>
     IntegrationSettings a
     -> Stepper m a
     -> a
     -> (a, [a])
     -> a
     -> Derivs a
     -> m ((Int, Int), [Step a [a]])

integrate IntegrationSettings{..} stepper h0 start@(x0,_) endX derivs = 
    runWriterT $ loop 0 h0 start (uncurry derivs start) 0 0
    where
        dt = endX - x0
        loop nstp hProposed stepStart@(x, ys) dydxs nok nbad
            | x == endX      = do
                tell [Step {from = stepStart, to = stepStart, hUsed = 0, dense = const ys}]
                return (nok, nbad)
            | nstp >= maxStp = fail "integrate: too many steps"
            | otherwise      = do
                let h = minBy (comparing abs) hProposed (endX - x)
                (newDydxs, hNext, step) <- lift (stepper derivs x dydxs ys h)
                tell [step]
                
                let (nok', nbad')
                        | hUsed step == h   = (nok+1, nbad)
                        | otherwise         = (nok, nbad+1)
                    (newX, _) = to step
                
                -- done?
                if (newX - endX) * dt >= 0
                    then return (nok', nbad')
                    else if abs hNext <= hMin
                        then fail "integrate: step size too small"
                        else loop (nstp + 1) hNext (to step) newDydxs 0 0-- nok' nbad'
