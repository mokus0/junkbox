module NR.Ch2.S8 where

import NR.Ch1.S4
import Control.Monad.ST
import Data.STRef

vander x q
    | vecElems x /= vecElems q  = error "vander: unequal length vectors"
    | n == 1    = q
    | otherwise = runST $ do
        w <- newVector_ n
        c <- newVector n $ \i -> if i == n-1 then negate (indexV x 0) else 0
        let _ = w `asTypeOf` c
        
        sequence_
            [ do
                let xx = negate (indexV x i)
                sequence_
                    [ do
                        c_jp1 <- readV c (j+1)
                        modifyV c j (+ xx * c_jp1)
                    | j <- [n-1-i .. n-2]
                    ]
                modifyV c (n-1) (+ xx)
            | i <- [1..n-1]
            ]
        
        sequence_
            [ do
                let xx = indexV x i
                t <- newSTRef 1
                b <- newSTRef 1
                s <- newSTRef (indexV q (n-1))
                sequence_
                    [ do
                        c_k <- readV c k
                        modifySTRef b (\b -> c_k + xx * b)
                        bVal <- readSTRef b
                        modifySTRef s (+ indexV q (k-1) * bVal)
                        modifySTRef t (\t -> xx*t + bVal)
                    | k <- [n-1, n-2 .. 1]
                    ]
                s <- readSTRef s
                t <- readSTRef t
                writeV w i (s/t)
            | i <- [0..n-1]
            ]
        
        
        unsafeFreezeVector w
    where
        n = vecElems q