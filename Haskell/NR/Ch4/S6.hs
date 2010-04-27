{-# LANGUAGE FlexibleContexts, RecordWildCards, ParallelListComp #-}
module NR.Ch4.S6 where

import Control.Monad.Loops
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

-- |returns the integral of the function between the 2 points, by ten-point
-- Gauss-Legendre integration.  The function is evaluated exactly 10 times
-- at interior points in the range of integration.
--
-- Equivalent to a GaussQ generated by gauleg using x1 = -1, x2 = 1 and n=10.
qgaus func a b = go xs ws 0
    where
        xs = [0.1488743389816312,0.4333953941292472,0.6794095682990244,0.8650633666889845,0.9739065285171717]
        ws = [0.2955242247147529,0.2692667193099963,0.2190863625159821,0.1494513491505806,0.0666713443086881]
        xm = 0.5*(b+a);
        xr = 0.5*(b-a);
        
        go [] [] s          = s * xr
        go (x:xs) (w:ws) s  = go xs ws (s + w * (func (xm+dx) + func (xm-dx)))
                where dx = xr * x

-- |Weights and abscissas for gaussian quadrature.
data GaussQ v a = GaussQ {abscissas :: v a, weights :: v a }
    deriving (Eq, Show)

-- |Apply a Gaussian quadrature rule to a function.  The endpoints of integration
-- are a property of the rule - typically, they were supplied to the function
-- (such as 'gauleg' below) that computed the parameters for the rule.
integrate GaussQ{..} f = sum [w * f x | x <- V.toList abscissas | w <- V.toList weights]

-- |Given the lower and upper limits of integration, number of points n, and
-- desired accuracy eps, this routine returns a GaussQ quadrature rule containing
-- the abscissas and weights of the Gauss-Legendre n-point quadrature formula.
--
-- Typical values for eps are 1e-15 for Double, 1e-6 for Float.  The specified
-- eps will be approximately the accuracy of the integration method as well.
gauleg x1 x2 n eps = runST $ do
    x <- MV.newWith n 0
    w <- MV.newWith n 0
    
    let -- the roots are symmetric in the interval so we only have to find 'm' of them
        m = (n + 1) `div` 2
        xm = 0.5*(x2+x1)
        xl = 0.5*(x2-x1)
    
    -- Loop over the desired roots
    sequence_
        [ do
            -- starting with this approximation to the ith root, enter main loop of
            -- refinement by Newton's method
            z  <- newSTRef . realToFrac $ cos (pi * (fromIntegral i + 0.75) / (fromIntegral n + 0.5))
            z1 <- newSTRef undefined
            pp <- newSTRef undefined
            
            let improve = do
                    p1 <- newSTRef 1
                    p2 <- newSTRef 0
                    z' <- readSTRef z
                    sequence_
                        [ do
                            -- loop up the recurrence relation to get the
                            -- Legendre polynomial evaluated at z
                            p3' <- readSTRef p2
                            p2' <- readSTRef p1
                            writeSTRef p2 p2'
                            let p1' = ((2 * fromIntegral j + 1) * z' * p2' - fromIntegral j * p3') / (fromIntegral j + 1)
                            writeSTRef p1 p1'
                            
                        | j <- [0..n-1]
                        ]
                    -- p1 is now the desired Legendre polynomial.  We next compute pp, its derivative,
                    -- by a standard relation involving also p2, the polynomial of one lower order.
                    p1 <- readSTRef p1
                    p2 <- readSTRef p2
                    let pp' = fromIntegral n * (z' * p1 - p2) / (z'*z' - 1)
                    writeSTRef pp pp'
                    
                    writeSTRef z1 z'
                    writeSTRef z (z' - p1/pp')
                converged = do
                    z  <- readSTRef z
                    z1 <- readSTRef z1
                    -- return (abs (z - z1) <= eps)
                    return (abs (z-z1) < eps)
            
            improve `untilM` converged
            
            z <- readSTRef z
            pp <- readSTRef pp
            let i' = n-1-i
            MV.write x i  (xm - xl * z)
            MV.write x i' (xm + xl * z)
            let w_i = 2 * xl / ((1 - z*z)*pp*pp)
            MV.write w i  w_i
            MV.write w i' w_i
            
        | i <- [0 .. m-1]
        ]
    
    abscissas <- V.unsafeFreeze x
    weights   <- V.unsafeFreeze w
    return GaussQ{..}