{-# LANGUAGE FlexibleContexts #-}
{-
 - Gauss-Jordan elimination with full pivoting
 -}
module NR.Ch2.S1 where

import NR.Ch1.S4
import NR.Ch1.S4.Alias

import Control.Monad
import Control.Monad.ST
import Data.Complex
import Data.StateRef
import Data.Permute.ST
import Data.Ord

import qualified Data.IntSet as S

import qualified Data.Vector.Generic as GV

-- examples:
-- type T = UMatrix (Complex Double)
-- a, b, x' :: T
-- a  = matrixFromList [[1,3,-2],[3,5,6],[2,4,3]]
-- b  = matrixFromList [[5],[7 :+ 2],[8]] 
-- x' = matrixFromList [[-15],[8],[2]]
-- aInv, x :: T
-- (aInv, x)  = gaussj_complex a b
-- b' :: T
-- b' = a `multiply` x

gaussj :: (Fractional t,
           Ord t,
           Matrix m1 t,
           Matrix m2 t, 
           GV.Vector v1 t,
           GV.Vector v2 t) =>
          m1 t -> m2 t -> (NRMatrix v1 t, NRMatrix v2 t)
gaussj a b = runST $ do
    a <- copyMatrix a 
    b <- copyMatrix b
    gaussj_generic (comparing abs) a b
    a <- unsafeFreezeMatrix a
    b <- unsafeFreezeMatrix b
    return (a,b)

gaussj_complex :: (RealFloat t,
           Ord t,
           Matrix m1 (Complex t),
           Matrix m2 (Complex t), 
           GV.Vector v1 (Complex t),
           GV.Vector v2 (Complex t)) =>
          m1 (Complex t) -> m2 (Complex t) -> (NRMatrix v1 (Complex t), NRMatrix v2 (Complex t))
gaussj_complex a b = runST $ do
    a <- copyMatrix a 
    b <- copyMatrix b
    gaussj_generic (comparing magnitude) a b
    a <- unsafeFreezeMatrix a
    b <- unsafeFreezeMatrix b
    return (a,b)

gaussj_inv :: (Fractional t, Ord t, Matrix m t, GV.Vector v t) => m t -> NRMatrix v t
gaussj_inv a = runST $ do
    a <- copyMatrix a
    gaussj_inv_generic (comparing abs) a >>= unsafeFreezeMatrix

gaussj_inv_complex :: (RealFloat t, Ord t, Matrix m (Complex t), GV.Vector v (Complex t)) => m (Complex t) -> NRMatrix v (Complex t)
gaussj_inv_complex a = runST $ do
    a <- copyMatrix a
    gaussj_inv_generic (comparing magnitude) a >>= unsafeFreezeMatrix

gaussj_inv_generic cmp a = do
    n <- getNumRows a
    b <- copyMatrix (FunctionMatrix n 0 undefined)
    gaussj_generic cmp a (b `asTypeOf` a)
    return a

gaussj_generic cmp a b = do
    n <- getNumRows a
    m <- getNumCols b
    
    -- the set of indices that have not yet been processed
    ipiv <- newRef (S.fromList [0..n-1])
    -- the permutation of the columns in the output
    indx <- newPermute n
    
    sequence_
        [ do
            -- pivot and return the column to work on.  icol will take
            -- each value 0..n-1 exactly once.
            icol <- pivot cmp a b n ipiv indx
            
            piv <- readM a icol icol
            when (piv == 0) $ fail "gaussj: Singular Matrix"
            let pivinv = recip piv
            
            -- scale the pivot row of A by the amount that would
            -- make the diagonal element 1.  Because the diagonal element
            -- is now part of the inverse, though, it will be set to pivinv.
            writeM a icol icol 1
            scaleRowM_n n a icol pivinv
            
            -- scale the corresponding row of the solution set
            scaleRowM_n m b icol pivinv
            
            -- do the "elimination" step
            reduceRows a b n m icol
        
        | i <- [0..n-1]
        ]
    
    -- put the columns of the inverse of A back in place, by inverting the
    -- permutation applied by the 'pivot' function.
    invPermuteColsM indx a

-- select a cell to pivot on.
-- place that element on the diagonal by swapping rows, and log the swap for
-- unshuffleColumns to apply to the same columns of the inverse later
pivot cmp a b n ipiv indx = do
    -- get the list of indices not yet processed, in no particular order
    is <- readsRef ipiv S.toList
    -- select the next pivot cell
    (irow, icol) <- selectPivot cmp a n is
    -- update the set of indices
    modifyRef ipiv (S.delete icol)
    
    -- swap the rows and update the column permutation to place the
    -- pivot element on the diagonal
    when (irow /= icol) $ do
        swapRowsM a irow icol
        swapRowsM b irow icol
        swapElems indx irow icol
    
    -- return the column pivoted on, so the rest of
    -- the algorithm knows where to find the diagonal.
    return icol

-- select the largest element in the unpivoted rows and columns of the matrix.
selectPivot cmp a n is = go is is 0 (error "no pivot selected")
    where
        -- after last row, return best found
        go      []    _    big irc = return irc
        
        -- after last col in a row, start the next row
        go    (j:js)  []   big irc = go js is big irc
        
        -- for each cell, compare value with best yet and move on.
        -- "big" stores best value found, for comparison.
        -- "irc" stores location of best cell found, for return at end.
        go js@(j:_) (k:ks) big irc = do
            x <- readM a j k
            if x `cmp` big /= LT
                then go js ks x  (j,k)
                else go js ks big irc

-- subtract linear combinations of the pivot row from every other row, using
-- the value that will cause all values in the column to become zero.  This
-- simultaneously updates the remaining columns of A and the already-produced
-- columns of its inverse.  The same operation is also done to B.
reduceRows a b n m icol = sequence_
    [ do
        iscale <- readM a i icol
        -- explicitly zero the element of A on the pivot column,
        -- in case of roundoff errors in reduceRow
        writeM a i icol 0
        
        -- perform the elimination for row i
        reduceRow a n iscale icol i
        reduceRow b m iscale icol i
    | i <- [0..n-1]
    , i /= icol
    ]

-- scale and subtract one row (icol) from another (ll) storing the result
-- in the 2nd row
reduceRow a n iscale icol ll = do
    let f x y = x - y * iscale
    a_ll   <- a `aliasRow` ll
    a_icol <- a `aliasRow` icol
    zipWithV_n n a_ll f a_ll a_icol
