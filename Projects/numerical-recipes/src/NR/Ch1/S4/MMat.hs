{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module NR.Ch1.S4.MMat where

import NR.Ch1.S4.IMat

import Control.Monad.Primitive
import Data.Permute
import Data.Permute.MPermute hiding (unsafeFreeze, unsafeThaw)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as UV
import qualified Data.Vector.Generic.Mutable as GV
import qualified Data.Vector.Generic as GV (unsafeFreeze)

class Monad m => MMatrix mat t m where
    newMatrix_ :: Int -> Int -> m (mat t)
    newMatrix  :: Int -> Int -> (Int -> Int -> t) -> m (mat t)
    readM :: mat t -> Int -> Int -> m t
    writeM :: mat t -> Int -> Int -> t -> m ()
    modifyM :: mat t -> Int -> Int -> (t -> t) -> m t
    modifyM m i j f = do
        x <- readM m i j
        let fx = f x
        writeM m i j fx
        return fx
    updateM :: mat t -> Int -> Int -> (t -> m t) -> m t
    updateM m i j f = do
        x <- readM m i j
        x <- f x
        writeM m i j x
        return x
    getMatSize :: mat t -> m (Int, Int)

copyMatrix m = newMatrix (matRows m) (matCols m) (indexM m)

getNumRows m = do
    (r,c) <- getMatSize m
    return r
getNumCols m = do
    (r,c) <- getMatSize m
    return c

swapRowsM a r1 r2 = do
    n <- getNumCols a
    sequence_
        [ do
            r1v <- readM a r1 i
            r2v <- readM a r2 i
            writeM       a r1 i r2v
            writeM       a r2 i r1v
        
        | i <- [0..n-1]
        ]

permuteRowsM m p = sequence_
    [ swapRowsM m r1 r2
    | (r1, r2) <- swaps p
    ]
    
invPermuteColsM indx a = do
    swaps <- getInvSwaps indx
    sequence_
        [ swapColsM a r c
        | (r, c) <- swaps
        , r /= c
        ]

swapColsM a c1 c2 = do
    n <- getNumRows a
    sequence_
        [ do
            c1v <- readM a i c1
            c2v <- readM a i c2
            writeM a i c1 c2v
            writeM a i c2 c1v
        
        | i <- [0..n-1]
        ]

scaleRowM     a r x = mapRowM     a r (* x)
scaleRowM_n n a r x = mapRowM_n n a r (* x)

mapRowM a r f = do
    n <- getNumCols a
    mapRowM_n n a r f

mapRowM_n n a r f = sequence_
    [ do
        v <- readM a r i
        writeM a r i (f v)
    | i <- [0..n-1]
    ]


type STMatrix  s = NRMatrix (V.MVector  s)
type STUMatrix s = NRMatrix (UV.MVector s)

instance (GV.MVector v a, PrimMonad m, s ~ PrimState m) => MMatrix (NRMatrix (v s)) a m where
    newMatrix_ r c = do
        let n = r*c
        m <- GV.new n
        return (NRMatrix r c m)
    newMatrix r c f = do
        let n = r*c
        m <- GV.new n
        sequence_
            [ GV.write m k (f i j)
            | i <- [0..r-1]
            , j <- [0..c-1]
            , let k = i*c+j
            ]
        return (NRMatrix r c m)
    readM (NRMatrix r c m) i j = GV.read m (i*c+j)
    writeM (NRMatrix r c m) i j x = GV.write m (i*c+j) x
    getMatSize (NRMatrix r c m) = return (r,c)

{-# INLINE unsafeFreezeMatrix #-}
unsafeFreezeMatrix (NRMatrix r c m) = do
    m <- GV.unsafeFreeze m
    return (NRMatrix r c m)
