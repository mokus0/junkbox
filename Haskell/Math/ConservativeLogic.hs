module Math.ConservativeLogic where

import Data.Ratio
import Math.Gamma

-- I had been contemplating a representation of conservative logic programs
-- based on an implicit enumeration, but some "back of the envelope" computations
-- show this to be quite infeasible.  A simple 8-bit duplication circuit
-- would take roughly 2.2898e47303 digits to store (based on indexing into
-- an implicit enumeration of 24-bit CLP circuits constrained to have 8
-- constant-1 inputs and 8 constant-0 inputs.
-- 
-- There is probably room to reduce that a bit by considering which of the
-- outputs are 'relevant' - eg, there are actually a great many valid CL programs
-- that do exactly the same thing up to permutation of the 'waste' outputs.
-- I doubt, though, that the reduction will be enough to make the concept practical.
-- 
-- For example, I estimate that in this case (16 waste bits, expected to be
-- about uniformly distributed between 1s and 0s) the savings is on the order of
-- @multinomialCoeff 16 [8,8]@, which is just 12870.

-- number of conservative linear programs with 'nBits' bits of input and output,
-- with the given number of 0 and 1 constants
numCLPs min0s min1s nBits = product
    [ factorial (multinomialCoeff nBits [n1s, n0s])
    | n1s <- [min1s..nBits-min0s]
    , let n0s = nBits - n1s
    ]

-- natural logarithm of 'numCLPs'
logNumCLPs min0s min1s nBits = sum
    [ lnFactorial (multinomialCoeff nBits [n1s, n0s])
    | n1s <- [min1s..nBits-min0s]
    , let n0s = nBits - n1s
    ]

multinomialCoeff :: Int -> [Int] -> Integer
multinomialCoeff n ks 
    | sum ks == n   = factorial n `div` product (map factorial ks)
    | otherwise     = error ("multinomialCoeffs: " ++ show n ++ " /= sum " ++ show ks)

