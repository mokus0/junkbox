{-# LANGUAGE RecordWildCards, ParallelListComp, ForeignFunctionInterface #-}
module Math.ODEInt where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer
import Data.List
import Data.Ord
import Data.STRef

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

test a x0 x1 = test' 1 (x0, [0,0,a]) x1

test' h0 (x0, ys0) x1 = runST $ do
    let eps = 1e-6
        derivs x [pos,vel,accel] = [vel, accel, 0]
    stepper <- stepperDopr5 (errorNorm (repeat eps) (repeat eps))
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings{hMin = 1e-8} stepper h0 (x0,ys0) x1 derivs
    return (nok, nbad, map sparse steps)

ln' x1 = runST $ do
    let h0 = signum (x1 - x0)
        x0 = 1
        ys0 = [0]
        derivs x [_] = [recip x]
        
        atol = [1e-16]
        rtol = [1e-16]
    stepper <- stepperDopr5 (errorNorm atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outLn]) = to (last steps)
    
    return $ if outX == x1
        then outLn
        else error ("ln': did not integrate all the way to x: " ++ show (map sparse steps))

lnTest x = ln' x / log x - 1

exp' x1 = runST $ do
    let h0 = signum (x1 - x0)
        x0 = 0
        ys0 = [1]
        derivs x [y] = [y]
        
        atol = [1e-16, 1]
        rtol = [1e-16, 1]
    stepper <- stepperDopr5 (errorNorm atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outY]) = to (last steps)
    
    return $ if outX == x1
        then outY
        else error ("exp': did not integrate all the way to x: " ++ show (map sparse steps))

expTest x = exp' x / exp x - 1

erf x1 | abs x1 > 6     = signum x1
erf x1 = min 1 . max (-1) . runST $ do
    let h0 = signum (x1 - x0)
        x0 = 0
        ys0 = [0]
        derivs x [y] = [(2 / sqrt pi) * exp (negate (x^2))]
        
        atol = [1e-37]
        rtol = [1e-19]
    stepper <- stepperDopr5 (errorNorm atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outY]) = to (last steps)
    
    return $ if outX == x1
        then outY
        else error ("erf: did not integrate all the way to x: " ++ show (map sparse steps))

erfc x1 = runST $ do
    let h0 = signum (x1 - x0)
        x0 = 0
        ys0 = [1]
        derivs x [y] = [((-2) / sqrt pi) * exp (negate (x^2))]
        
        atol = [1e-37]
        rtol = [1e-19]
    stepper <- stepperDopr5 (errorNorm atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outY]) = to (last steps)
    
    return $ if outX == x1
        then outY
        else error ("erfc: did not integrate all the way to x: " ++ show (map sparse steps))

foreign import ccall "math.h erf" c_erf :: Double -> Double
foreign import ccall "math.h erfc" c_erfc :: Double -> Double

erfTest x = erf x / c_erf x - 1

test2 a x0 = test2' 1 (x0, [0,0,a])
test2' h0 (x0, ys0) = runST $ do
    let eps = 1e-6
        derivs x [pos,vel,accel] = [vel, accel, 0]
    stepper <- stepperDopr5 (errorNorm (repeat eps) (repeat eps))
    (a, b, step@Step{to=(x1,ys1)})  <- stepper derivs x0 (derivs x0 ys0) ys0 h0
    (c, d, step2) <- stepper derivs x1 a ys1 b
    return [ (a, b, from step,  to step,  hUsed step)
           , (c, d, from step2, to step2, hUsed step2)
           ]