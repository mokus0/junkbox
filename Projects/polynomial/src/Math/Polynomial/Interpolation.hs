{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Interpolation where

import Math.Polynomial
import Data.List

-- |Evaluate a polynomial passing through the specified set of points.  The
-- order of the interpolating polynomial will (at most) be one less than 
-- the number of points given.
polyInterp :: Fractional a => [(a,a)] -> a -> a
polyInterp xys = head . last . neville xys

-- |Computes the tableau generated by Neville's algorithm.  Each successive
-- row of the table is a list of interpolants one order higher than the previous,
-- using a range of input points starting at the same position in the input
-- list as the interpolant's position in the output list.
neville :: Fractional a => [(a,a)] -> a -> [[a]]
neville xys x = table
    where
        (xs,ys) = unzip xys
        table = ys :
            [ [ ((x - x_j) * p0 + (x_i - x) * p1) / (x_i - x_j)
              | p1:p0:_ <- tails row
              | x_j     <- xs
              | x_i     <- x_is
              ]
            | row  <- table
            | x_is <- tail (tails xs)
            , not (null x_is)
            ]
