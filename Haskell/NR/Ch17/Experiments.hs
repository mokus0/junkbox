{-# LANGUAGE ForeignFunctionInterface #-}
module NR.Ch17.Experiments where

import NR.Ch17.S0
import NR.Ch17.S2
import NR.Ch17.WebNote20

import Control.Monad.ST (runST)

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
        
        atol = [1e-12]
        rtol = [1e-12]
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
        
        atol = [1e-12]
        rtol = [1e-12]
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


-- some Dopr853 tests
erf853 x1 | abs x1 > 6     = signum x1
erf853 x1 = min 1 . max (-1) . runST $ do
    let h0 = signum (x1 - x0)
        x0 = 0
        ys0 = [0]
        derivs x [y] = [(2 / sqrt pi) * exp (negate (x^2))]
        
        atol = [1e-12]
        rtol = [1e-12]
    stepper <- stepperDopr853 (errorNorm853 atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outY]) = to (last steps)
    
    return $ if outX == x1
        then outY
        else error ("erf853: did not integrate all the way to x: " ++ show (map sparse steps))

-- this erfc is not really satisfactory.  I just can't get
-- the integrator to put out a function with the required dynamic range.
-- there may be some reparameterization that would work - intuitively it
-- seems to me that the problem is that very long stretches have many 
-- infinitesimal steps (the integrator can't leap through these regions 
-- because they defy taylor series expansion).  Those steps are, beyond
-- some point, always smaller than the epsilon for the current value of
-- the integrand, and so the function hits a constant point and stays 
-- there.  Some kind of an exponential or hyperexponential 
-- reparameterization of x may be enough to allow the many steps to have
-- large enough derivatives to make progress through the same regions,
-- even if not putting the function in a form suitable for larger steps.
erfc853 x | x <= 5 = 1 - erf853 x
erfc853 x1 = runST $ do
    let h0 = signum (x1 - x0)
        x0 = 10
        ys0 = [c_erfc x0]
        derivs x [y] = [((-2) / sqrt pi) * exp (negate (x^2))]
        
        atol = [1e-100]
        rtol = [1e-12]
    stepper <- stepperDopr853 (errorNorm853 atol rtol)
    ((nok, nbad), steps) <- integrate defaultIntegrationSettings stepper h0 (x0,ys0) x1 derivs
    
    let (outX, [outY]) = to (last steps)
    
    return $ if outX == x1
        then outY
        else error ("erfc853: did not integrate all the way to x: " ++ show (map sparse steps))

erfTest853 x = erf853 x / c_erf x - 1
