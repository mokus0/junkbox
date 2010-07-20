{-# LANGUAGE
        MagicHash, UnboxedTuples
  #-}
module Functions.MergeSort where

import Data.Array.Simple
import Data.Bits

import Control.Monad
import Control.Monad.ST

a ! i = marray_read a i

copy_array a = array_new (array_size a) (array_index a)

mergeSort a = runST $ do
    a <- array_thaw (copy_array a)
    
    marray_merge_sort a
    marray_freeze a

marray_merge_sort a = go 0 sz
    where
        sz = marray_size a
        
        go off 0 = return ()
        go off 1 = return ()
        go off 2 = do
            x <- a ! off
            y <- a ! (off + 1)
            when (x>y) $ do
                marray_write a  off    y
                marray_write a (off+1) x
        go off n = do
            let p = n `shiftR` 1
                q = n - p
            go  off    p
            go (off+p) q
            merge off p (p+q)
        
        merge off p q = undefined