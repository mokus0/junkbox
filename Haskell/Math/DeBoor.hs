{-# LANGUAGE ParallelListComp #-}
module Math.DeBoor where

import Data.List
import Data.VectorSpace

interp a x y = (1-a) *^ x ^+^ a *^ y

alphas n x us = zipWith take 
    [n, n-1 .. 1]
    [ [ (x - u_i) / (u_j - u_i)
      | u_i <- u_is
      | u_j <- drop n us
      ]
    | u_is <- tails us
    ]

deBoor n x us ds = table
    where
        table = take (n+1) ds :
            [ [ interp alpha d0 d1
              | alpha <- alpha_ks
              | d0:d1:_ <- tails row
              ]
            | alpha_ks <- alphas n x us
            | row <- table
            ]

bspline n x us ds 
    | l   < 0   = error "bspline: x outside knot vector"
    | l+1 < n   = error "bspline: not enough knots below x"
    | l+n >= length us
                = error "bspline: not enough knots above x"
    | l >= length ds
                = error "bspline: not enough control points"
    | otherwise = head . last $ deBoor n x us' ds'
    where
        us' = drop (l-n+1) us
        ds' = drop (l-n+1) ds
        
        l = case findIndex (> x) us of
            Nothing -> case findIndex (== x) us of
                Nothing -> error "bspline: x outside knot vector"
                Just i -> i-1
            Just i  -> i-1
        
