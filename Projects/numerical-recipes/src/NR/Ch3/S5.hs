module NR.Ch3.S5 where

import Math.Polynomial

import NR.Ch3.S1
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import Control.Monad.ST
import Data.STRef

polyInterp table n = Interp table n (\tab j x -> fitPoly tab)

fitPoly table = poly LE (V.toList (polcoe table))

polcoe table = runST $ do
    let n = GV.length table
        x i = fst (table GV.! i)
        y i = snd (table GV.! i)
    
    s   <- MV.newWith n 0
    cof <- MV.newWith n 0
    let _ = s `asTypeOf` cof
    
    MV.write s (n-1) (negate (x 0))
    sequence_
        [ do
            sequence_
                [ do
                    s_j   <- MV.read s j
                    s_jp1 <- MV.read s (j+1)
                    MV.write s j $
                        s_j - x i * s_jp1
                | j <- [n-i-1 .. n-2]
                ]
            s_nm1 <- MV.read s (n-1)
            MV.write s (n-1) $
                s_nm1 - x i
        | i <- [1..n-1]
        ]
    
    sequence_
        [ do
            phi <- newSTRef (fromIntegral n)
            sequence_
                [ do
                    s_k <- MV.read s k
                    modifySTRef phi $ \phi -> fromIntegral k * s_k + x j * phi
                | k <- [n-1, n-2 .. 1]
                ]
            phi <- readSTRef phi
            let ff = y j / phi
            
            b <- newSTRef 1
            sequence_
                [ do
                    cof_k <- MV.read cof k
                    bVal <- readSTRef b
                    MV.write cof k (cof_k + bVal*ff)
                    
                    s_k <- MV.read s k
                    writeSTRef b (s_k + x j * bVal)
                | k <- [n-1, n-2 .. 0]
                ]
            
        | j <- [0 .. n-1]
        ]
    
    GV.unsafeFreeze cof