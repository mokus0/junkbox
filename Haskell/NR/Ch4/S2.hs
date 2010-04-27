{-# LANGUAGE ParallelListComp, BangPatterns #-}
module NR.Ch4.S2 where

import Control.Monad.ST
import Data.Bits

trapzd f a b = go 1 (0.5 * (b - a) * (f a + f b))
    where
        go !n !s = s : go (n+1) (0.5 * (s + dx * sum ys))
            where
                iters = bit (n-1) :: Int
                dx = (b-a) / fromIntegral iters
                x0 = a + 0.5 * dx
                ys = take iters [f x | x <- iterate (+dx) x0]

simp f a b = [(4/3) * s_2n - (1/3) * s_n | s_n <- trapzds | s_2n <- tail trapzds]
    where
        trapzds = trapzd f a b

qtrap f a b eps = qconv "qtrap" 5 20 eps (trapzd f a b)
qsimp f a b eps = qconv "qsimp" 5 20 eps (simp f a b)

-- general-purpose warm-start convergence test
qconv loc jstart jmax eps = converge jstart . drop (jstart-1)
    where
        converge j (x1:xs@(x2:_)) 
            | j >= jmax                     = error (loc ++ ": too many steps")
            | x1 == 0                       = 0
            | abs (x1-x2) <= eps * abs x1   = x2
            | otherwise                     = converge (j+1) xs