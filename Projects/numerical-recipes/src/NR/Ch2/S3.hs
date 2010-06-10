{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
{-
 - LU decomposition and related stuff, translated from 
 - C++ code given in Numerical Recipes, 3rd ed. (p. 52) 
 -}
module NR.Ch2.S3
    ( LUDecomp(..)
    , unsafeFreezeLUDecomp
    , lu_split
    , luImproveM
    , luInv, det
    , luSolveM, luSolveV
    , ludcmp, ludcmp_complex
    , ludcmp_generic
    , ludcmp_generic_nopivot
    ) where

import NR.Ch1.S4
import NR.Ch1.S4.Alias
import NR.Ch2.S2

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Permute
import Data.Permute.ST
import Data.StateRef

import Data.List
import Data.Ord
import Data.Complex

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GV

-- Example usage:
-- type M = UMatrix
-- type T = Double
-- a, b, x' :: M T
-- a  = matrixFromList [[1,3,-2],[3,5,6],[2,4,3]]
-- b  = matrixFromList [[5],[7],[8]] 
-- x' = matrixFromList [[-15],[8],[2]]
-- x :: IMatrix T
-- x  = luSolveM dcmp b
-- dcmp :: LUDecomp M T
-- dcmp = ludcmp a

data LUDecomp m t = LUDecomp
    { ludcmp_lu     :: m t
    , ludcmp_indx   :: Maybe Permute
    , ludcmp_d      :: Bool
    }

unsafeFreezeLUDecomp (LUDecomp lu indx d) = do
    lu <- unsafeFreezeMatrix lu
    return (LUDecomp lu indx d)

lu_split :: (Matrix m t, Num t) => LUDecomp m t -> (FunctionMatrix t, FunctionMatrix t)
lu_split (LUDecomp lu indx d) = (l,u)
    where
        n = matRows lu
        p = maybe id at indx
        l = matrix n n $ \i j -> case i `compare` j of
            LT -> 0
            EQ -> 1
            GT -> indexM lu (p i) j
        u = matrix n n $ \i j -> if i > j
            then 0
            else indexM lu (p i) j

luImproveV a dcmp b x0 = x0 `subV` convertV dx
    where
        dx = luSolveV dcmp rhs
        rhs = a `genericApply` x0 `subV` convertV b
        subV = liftVector2 (-)

luImproveM a dcmp b x0 = x0 `subM` convertM dx
    where
        dx = luSolveM dcmp rhs
        rhs = a `genericMultiply` x0 `subM` convertM b
        subM = liftMatrix2 (-)

luInv a = luSolveM (ludcmp' a) b
    where
        ludcmp' :: (Fractional a, Ord a, Matrix m a) => m a -> LUDecomp IMatrix a
        ludcmp' = ludcmp
        
        n = matRows a
        b = kronecker n `asTypeOf` a

det :: (Fractional a, Ord a, Matrix m a) => m a -> a
det a = case ludcmp a `asTypeOf` (undefined :: LUDecomp IMatrix a) of
    LUDecomp lu _ d -> sign (reduceDiagonal product lu)
        where
            sign | d         = id
                 | otherwise = negate

luSolveV dcmp = case ludcmp_indx dcmp of
    Nothing     -> backSub  u . forwardSub  l
    Just indx   -> backSub  u . forwardSub  l . permuteV indx
    where (l,u) = lu_split    dcmp
luSolveM dcmp = case ludcmp_indx dcmp of
    Nothing     -> backSubs  u . forwardSubs  l
    Just indx   -> backSubs  u . forwardSubs  l . permuteRows indx
    where (l,u) = lu_split    dcmp

ludcmp :: (Fractional a, Ord a, Matrix m a, GV.Vector v a) =>
          m a -> LUDecomp (NRMatrix v) a
ludcmp a = runST $ do
    lu <- copyMatrix a
    dcmp <- ludcmp_generic (comparing abs) lu
    unsafeFreezeLUDecomp dcmp

ludcmp_complex :: (RealFloat a, Matrix m (Complex a), GV.Vector v (Complex a)) =>
          m (Complex a) -> LUDecomp (NRMatrix v) (Complex a)
ludcmp_complex a = runST $ do
    lu <- copyMatrix a
    dcmp <- ludcmp_generic (comparing (\(a :+ b) -> a*a + b*b)) lu -- `asTypeOf` (undefined :: ST s (LUDecomp (STMatrix s) t))
    unsafeFreezeLUDecomp dcmp

ludcmp_generic_nopivot lu = do
    n <- getNumRows lu
    
    sequence_
        [ do
            piv <- readM lu k k
            when (piv == 0) $ fail "ludcmp: Singular Matrix"
            let pivInv = recip piv
            
            sequence_
                [ do
                    temp <- modifyM lu i k (*pivInv)
                    
                    sequence_
                        [ updateM lu i j $ \t -> do
                            lu_k_j <- readM lu k j
                            return (t - temp * lu_k_j)
                        | j <- [k+1..n-1]
                        ]
                | i <- [k+1..n-1]
                ]
        | k <- [0..n-1]
        ]

ludcmp_generic cmp luRaw = do
    n <- getNumRows luRaw
    
    vvRaw <- rowReduceM luRaw $ \xs -> do
        -- compute scaling factors for each row for use in implicit pivoting
        let x = maximumBy cmp xs
        when (x == 0) $ fail "ludcmp: Singular Matrix"
        return (recip x)
        
    indx <- newPermute n
    d  <- newRef True    -- permutation parity: True = even
    
    -- lu and vv are permuted versions of luRaw and vvRaw.
    -- The permutation is "live": whenever indx changes, so do these.
    lu <- aliasMatrixWith (RowMPermute indx) luRaw
    vv <- aliasVectorWith (VecMPermute indx) vvRaw
    
    sequence_
        [ do
            pivot cmp indx d lu vv n k
            
            piv <- readM lu k k
            when (piv == 0) $ fail "ludcmp: Singular Matrix"
            let pivInv = recip piv
            
            sequence_
                [ do
                    temp <- modifyM lu i k (*pivInv)
                    
                    sequence_
                        [ updateM lu i j $ \t -> do
                            lu_k_j <- readM lu k j
                            return (t - temp * lu_k_j)
                        | j <- [k+1..n-1]
                        ]
                | i <- [k+1..n-1]
                ]
        | k <- [0..n-1]
        ]
    
    indx <- unsafeFreeze indx
    d <- readRef d
    
    return (LUDecomp luRaw (Just indx) d)

pivot cmp indx d lu vv n k = do
    imax <- selectPivot cmp lu vv n k
    
    -- execute pivot
    when (k /= imax) $ do
        modifyRef d not
        swapElems indx k imax -- also swaps rows in lu and elems in vv

selectPivot cmp lu vv n k = go k k 0
    where
        go i imax big
            | i >= n    = return imax
            | otherwise = do
                v <- readV vv i
                x <- readM lu i k
                let temp = v * x
                case temp `cmp` big of
                    GT -> go (i+1) i    temp
                    _  -> go (i+1) imax big

rowReduceM :: (GV.MVector v t1, GV.MVector v t2, PrimMonad m, s ~ PrimState m) => NRMatrix (v s) t1 -> ([t1] -> m t2) -> m (NRVector (v s) t2)
rowReduceM m f = do
    (r,c) <- getMatSize m
    vs <- sequence
        [ do
            xs <- mapM (readM m i) [0..c-1]
            f xs
        | i <- [0..r-1]
        ]
    newVector r (vs!!)

reduceDiagonal f m = f [indexM m i i | i <- [0..min (matRows m) (matCols m) - 1]]