{-# OPTIONS -fno-monomorphism-restriction #-}

module Main where

import Control.Monad.State

-- version in the text uses "shift-right" on a value specified as 1 <= x < 2
-- I have changed these to division operations, since there's no canned
-- instance for Bits Double or Bits Float
-- import Bits

stop = do
    (b, x, y, z, k) <- get
    return y

l1 b x = do
    let y = 0
    -- let z = x `shiftR` 1
    let z = x / 2
    let k = 1
    put (b, x, y, z, k)
    l2
  
l2 = do
    (b, x, y, z, k) <- get
    if x == 1
	then
	    stop
	else
	    l2a

l2a = do
    (b, x, y, z, k) <- get
    if x - z == x
	then
	    fail "x - z == x, but x /= 1"
	else
	    l3
l3 = do
    (b, x, y, z, k) <- get
    if (x - z) < 1
	then do
	    -- let z = z `shiftR` 1
	    let z = z / 2
	    let k = k + 1
	    put (b, x, y, z, k)
	    l3
	else do
	    l4

l4 = do
    (b, x, y, z, k) <- get
    let x = x - z
    -- let z = x `shiftR` k
    let z = x / (2^k)
	   
    let lb n = (log n) / (log b)
    let lbk k = lb ((2^k)/((2^k) - 1))
    let y = y + (lbk k)
    
    put (b, x, y, z, k)
    l2

logb b x
    | x < 1	= error "x must be in range [1,2)"
    | x >= 2	= error "x must be in range [1,2)"
    | otherwise	= evalState (l1 b x) undefined

lg = logb 2
