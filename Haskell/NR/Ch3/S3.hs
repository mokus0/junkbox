{-# LANGUAGE
        FlexibleContexts
  #-}
module NR.Ch3.S3 where

import NR.Ch3.S1

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import Control.Monad.ST

-- |Cubic spline interpolation, inferring the 2nd derivatives to use (optionally
-- specifying the first derivatives at the endpoints)
splineInterp table yp1 ypn = Interp table 2 (splineRawInterp y2)
    where y2 = mkDerivs table yp1 ypn

-- |Cubic spline interpolation with explicitly specified 2nd derivatives.
-- Each row of the table should contain (x, y, y'').
splineInterp' table = Interp xys 2 (splineRawInterp y2)
    where
        (xys, y2) = GV.unzip (GV.map (\(x,y,y2) -> ((x,y), y2)) table)

mkDerivs :: (GV.Vector v (a, a), GV.Vector v a, Fractional a) =>
     v (a, a) -> Maybe a -> Maybe a -> v a
mkDerivs table yp1 ypn = runST $ do
    let n = GV.length table
    y2 <- MV.new n
    u  <- MV.new (n-1) `asTypeOf` return y2
    
    let xv i = fst (table GV.! i)
        yv i = snd (table GV.! i)
    
    case yp1 of
        Nothing -> do
            MV.write y2 0 0
            MV.write u  0 0
        Just d  -> do
            MV.write y2 0 (-0.5)
            MV.write u  0 $ 
                (3 / (xv 1 - xv 0)) * ((yv 1 - yv 0) / (xv 1 - xv 0) - d)
    
    sequence_
        [ do
            y2_im1 <- MV.read y2 (i-1)
            let sig = (xv i - xv (i-1)) / (xv (i+1) - xv (i-1))
                p = sig * y2_im1 + 2
            MV.write y2 i ((sig - 1) / p)
            let t = (yv (i+1) - yv i) / (xv (i+1) - xv i) - (yv i - yv (i-1)) / (xv i - xv (i-1))
            u_im1 <- MV.read u (i-1)
            MV.write u i $ (6 * t / (xv (i+1) - xv (i-1)) - sig * u_im1) / p
        | i <- [1 .. n-2]
        ]
    
    let (qn, un) = case ypn of
            Nothing -> (0, 0)
            Just d  -> (0.5, (3 / xv (n-1) - xv (n-2)) * (d - (yv (n-1) - yv (n-2)) / (xv (n-1) - xv (n-2))))
    
    u_nm2   <- MV.read u  (n-2)
    y2_nm2  <- MV.read y2 (n-2)
    MV.write y2 (n-1) $ 
        (un - qn * u_nm2) / (qn * y2_nm2 + 1)
    
    sequence_
        [ do
            y2_k    <- MV.read y2 k
            y2_kp1  <- MV.read y2 (k+1)
            u_k     <- MV.read u k
            
            MV.write y2 k (y2_k * y2_kp1 + u_k)
            
        | k <- [n-2, n-3 .. 0]
        ]
    
    GV.unsafeFreeze y2

splineRawInterp y2 xys j x
    | h == 0    = error "splineInterp: double abscissa"
    | otherwise = y
    where
        h = x1 - x0
        a = (x1 - x) / h
        b = (x - x0) / h
        y = a*y0 + b*y1 + ((a^3-a)*y2_0 + (b^3-b)*y2_1) * (h*h/6)
        [y2_0, y2_1] = GV.toList (GV.slice j (GV.length xys) y2)
        [(x0,y0), (x1,y1)] = GV.toList xys