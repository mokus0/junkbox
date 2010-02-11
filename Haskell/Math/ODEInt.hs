{-# LANGUAGE RecordWildCards, ParallelListComp #-}
module Math.ODEInt where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer
import Data.STRef
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

stepperDopr5 :: (Floating a, Ord a) => [a] -> [a] -> ST s (Stepper (ST s) a)
stepperDopr5 atols rtols = do
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
        dense = undefined
        
        errorNorm ys newYs delta = sqrt (err / fromIntegral (length ys))
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
        beta = 0.0
        safe = 0.9
        minscale = 0.2
        maxscale = 10
        
dy derivs dydxs x ys h 
    | newX == x = fail "stepperDopr5: stepsize underflow"
    | otherwise = return (newDydxs, newX, newYs, delta)
    where
        -- constants:
        c2 = 0.2; c3 = 0.3; c4 = 0.8; c5 = 8/9
        a21 = 0.2
        a31 = 3/40;       a32 = 9/40
        a41 = 44/45;      a42 = (-56)/15;      a43 = 32/9
        a51 = 19372/6561; a52 = (-25360)/2187; a53 = 64448/6561;  a54 = (-212)/729
        a61 = 9017/3168;  a62 = (-355)/33;     a63 = 46732/5247;  a64 = 49/176;    a65 = (-5103)/18656
        a71 = 35/384;                          a73 = 500/1113;    a74 = 125/192;   a75 = (-2187)/6784;    a76 = 11/84
        e1  = 71/57600;                        e3  = (-71)/16695; e4  = 71/1920;   e5  = (-17253)/339200; e6  = 22/525; e7 = recip (-40)
        
        -- computations:
        newX = x + h
        
        k2s = derivs (x + c2*h) 
            [ y + h * (a21*dydx)
            | y     <- ys
            | dydx  <- dydxs
            ]
        
        k3s = derivs (x + c3*h) 
            [ y + h * (a31*dydx + a32*k2)
            | y     <- ys
            | dydx  <- dydxs
            | k2    <- k2s
            ]
        
        k4s = derivs (x + c4*h) 
            [ y + h * (a41*dydx + a42*k2 + a43*k3)
            | y     <- ys
            | dydx  <- dydxs
            | k2    <- k2s
            | k3    <- k3s
            ]
        
        k5s = derivs (x + c5*h) 
            [ y + h * (a51*dydx + a52*k2 + a53*k3 + a54*k4)
            | y     <- ys
            | dydx  <- dydxs
            | k2    <- k2s
            | k3    <- k3s
            | k4    <- k4s
            ]
        
        k6s = derivs newX
            [ y + h * (a61*dydx + a62*k2 + a63*k3 + a64*k4 + a65*k5)
            | y     <- ys
            | dydx  <- dydxs
            | k2    <- k2s
            | k3    <- k3s
            | k4    <- k4s
            | k5    <- k5s
            ]
        
        newDydxs = derivs newX newYs
        newYs = 
            [ y + h * (a71*dydx + a73*k3 + a74*k4 + a75*k5 + a76*k6)
            | y     <- ys
            | dydx  <- dydxs
            | k3    <- k3s
            | k4    <- k4s
            | k5    <- k5s
            | k6    <- k6s
            ]
        
        delta = 
            [ h * (e1*dydx + e3*k3 + e4*k4 + e5*k5 + e6*k6 + e7*newDydx)
            | dydx    <- dydxs
            | k3      <- k3s
            | k4      <- k4s
            | k5      <- k5s
            | k6      <- k6s
            | newDydx <- newDydxs
            ]

test a x0 x1 = test' 1 (x0, [0,0,a]) x1

test' h0 (x0, ys0) x1 = runST $ do
    let eps = 1e-6
        derivs x [pos,vel,accel] = [vel, accel, 0]
    stepper <- stepperDopr5 (repeat eps) (repeat eps)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings{hMin = 1e-8} stepper h0 (x0,ys0) x1 derivs
    return (nok, nbad, map sparse steps)

ln' x1 = runST $ do
    let h0 = signum (x1 - x0)
        x0 = 1
        ys0 = [0,1]
        derivs x [ln, y] = [recip y, 1]
        
        atol = [1e-16, 1]
        rtol = [1e-16, 1]
    stepper <- stepperDopr5 atol rtol
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outLn, _]) = to (last steps)
    
    return $ if outX == x1
        then outLn
        else error ("ln': did not integrate all the way to x: " ++ show (map sparse steps))

lnTest x = ln' x - log x

test2 a x0 = test2' 1 (x0, [0,0,a])
test2' h0 (x0, ys0) = runST $ do
    let eps = 1e-6
        derivs x [pos,vel,accel] = [vel, accel, 0]
    stepper <- stepperDopr5 (repeat eps) (repeat eps)
    (a, b, step@Step{to=(x1,ys1)})  <- stepper derivs x0 (derivs x0 ys0) ys0 h0
    (c, d, step2) <- stepper derivs x1 a ys1 b
    return [ (a, b, from step,  to step,  hUsed step)
           , (c, d, from step2, to step2, hUsed step2)
           ]