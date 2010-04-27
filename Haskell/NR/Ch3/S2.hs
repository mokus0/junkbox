{-# LANGUAGE ParallelListComp #-}
module NR.Ch3.S2 where

import NR.Ch3.S1

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import Data.Ord
import Control.Monad
import Control.Monad.ST
import Data.STRef

polyInterp table m = Interp table m polyRawInterp

-- NB: this algorithm works for either ascending or descending ordered abscissas
polyRawInterp v j x = runST $ do
    -- Initialize c and d arrays to Y values
    c <- MV.new mm
    d <- MV.new mm
    
    -- Constrain types to use same vector internally as we are passed
    _ <- return (GV.map snd v) `asTypeOf` GV.unsafeFreeze (c `asTypeOf` d)
    
    sequence_
        [ do
            MV.write c i y
            MV.write d i y
        | (_,y) <- GV.toList v
        | i <- [0..]
        ]
    
    y  <- newSTRef (snd (v GV.! ns))
    dy <- newSTRef 0
    ns <- newSTRef (ns - 1)
    
    sequence_
        [ do
            sequence_
                [ do
                    let ho = xo - x
                        hp = xp - x
                        den' = ho - hp
                    when (den' == 0) (fail "polyRawInterp: double abscissa")
                    w <- liftM2 (-) (MV.read c (i+1)) (MV.read d i)
                    let den = w / den'
                    MV.write d i (hp * den)
                    MV.write c i (ho * den)
                | i <- [0 .. mm-m-1]
                | (xo, _) <- GV.toList v
                | (xp, _) <- GV.toList (GV.drop m v)
                ]
            yVal <- readSTRef y
            nsVal <- readSTRef ns
            
            dyVal <- if 2 * (nsVal + 1) < mm - m
                then MV.read c (nsVal+1)
                else do
                    modifySTRef ns (subtract 1)
                    MV.read d nsVal
            modifySTRef y (+ dyVal)
            writeSTRef dy dyVal
        | m <- [1 .. mm-1]
        ]
    
    y <- readSTRef y
    dy <- readSTRef dy
    return (y, dy)
    
    where
        mm  = GV.length v
        ns = GV.minIndexBy (comparing (\(x_i, y_i) -> abs (x - x_i))) v
