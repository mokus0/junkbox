{-# LANGUAGE ParallelListComp, RecordWildCards #-}
module NR.Ch17.S2 (stepperDopr5, errorNorm) where

import NR.Ch17.S0

import Data.List

import Control.Monad.ST
import Data.STRef


errorNorm atols rtols ys newYs delta = sqrt (err / fromIntegral (length ys))
    where
        err = sum 
            [ let sk = atol + rtol * max (abs y) (abs newY)
               in (yerr/sk)^2
            | y     <- ys
            | newY  <- newYs
            | yerr  <- delta
            | atol  <- atols ++ repeat 0
            | rtol  <- rtols ++ repeat 0
            ]

-- fifth-order adaptive runge-kutta stepper
stepperDopr5 :: (Floating a, Ord a) => ([a] -> [a] -> [a] -> a) -> ST s (Stepper (ST s) a)
stepperDopr5 errorNorm = do
    reject <- newSTRef False
    return (step reject)
    where
        step reject derivs x dydxs ys h = do
            (newDydxs, newX, newYs, delta) <- dy derivs dydxs x ys h
            controllerResult <- controller reject (errorNorm ys newYs delta) h
            case controllerResult of
                {- step rejected -} 
                Left hNew -> step reject derivs x dydxs ys hNew
                
                {- step accepted -} 
                Right hNext -> finish newDydxs (x,ys) (newX, newYs) h hNext
        
        finish dydxs from to hUsed hnext =
            return (dydxs, hnext, Step {..})
        dense = error "stepperDopr5: dense output not yet implemented"
        
        controller reject err h
            | err <= 1      = do
                {- accept -}
                let scale   | err == 0  = maxscale
                            | otherwise = min maxscale 
                                        . max minscale 
                                        $ safe * (err ** negate alpha) * (err ** beta)
                r <- readSTRef reject
                let hNext | r         = h * min scale 1
                          | otherwise = h * scale
        
                writeSTRef reject False
                return (Right hNext)
            | otherwise     = do
                {- reject -}
                let scale = max (safe * (err ** negate alpha)) minscale
                writeSTRef reject True
                return (Left (h * scale))
            where
                alpha = 0.2 - beta * 0.75
                beta = 0.08
                safe = 0.9
                minscale = 0.2
                maxscale = 10
        
        dy derivs dydxs x ys h 
            | newX == x = fail "stepperDopr5: stepsize underflow"
            | otherwise = return (newDydxs, newX, newYs, delta)
            where
                -- constants:
                c @[c2,  c3,  c4,  c5] = [0.2, 0.3, 0.8, 8/9]
                a = [ [0.2]
                    , [3/40,       9/40]
                    , [44/45,      (-56)/15,      32/9]
                    , [19372/6561, (-25360)/2187, 64448/6561, (-212)/729]
                    , [9017/3168,  (-355)/33,     46732/5247, 49/176,     (-5103)/18656]
                    , [35/384,     0,             500/1113,   125/192,    (-2187)/6784,   11/84]
                    ]
                e = [71/57600, 0, (-71)/16695, 71/1920, (-17253)/339200, 22/525, recip (-40)]
        
                [a2, a3, a4, a5, a6, a7] = a
        
                -- computations:
                k = transpose
                    [ dydxs
                    , derivs (x + c2*h) (vadd ys (vscale h (map (vdot a2) k)))
                    , derivs (x + c3*h) (vadd ys (vscale h (map (vdot a3) k)))
                    , derivs (x + c4*h) (vadd ys (vscale h (map (vdot a4) k)))
                    , derivs (x + c5*h) (vadd ys (vscale h (map (vdot a5) k)))
                    , derivs newX       (vadd ys (vscale h (map (vdot a6) k)))
                    , newDydxs
                    ]
                newX = x + h
                newYs = vadd ys (vscale h (map (vdot a7) k))
                newDydxs = derivs newX newYs
        
                delta = vscale h (map (vdot e) k)

vadd v1 v2 = zipWith (+) v1 v2
vscale s v = map (s*) v
vdot v1 v2 = foldl' (+) 0 (zipWith (*) v1 v2)
