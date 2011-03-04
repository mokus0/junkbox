module Translations.FifthRoot where

-- (sic): the names are backwards
fifthRootWrong x = fixedPointOfTransform (\y -> x / y^4) (repeated averageDamp 2) 1
fifthRootRight x = fixedPoint (repeated (averageDamp (\y -> x / y^4)) 2) 1

average x y = 0.5 * (x + y)
averageDamp f x = average x (f x)

tolerance = 0.00001

fixedPoint f firstGuess = try firstGuess
    where
        closeEnough v1 v2 = abs (v1-v2) < tolerance
        try guess
            | closeEnough guess next    = next
            | otherwise                 = try next
            where next = f guess

fixedPointOfTransform g transform guess = fixedPoint (transform g) guess

repeated f 1 = f
repeated f n = f . repeated f (n-1)


-- expanded:

-- fifthRootWrong x = fixedPoint (repeated averageDamp 2 (\y -> x / y^4)) 1
-- fifthRootWrong x = fixedPoint (averageDamp (averageDamp (\y -> x / y^4))) 1
-- fifthRootWrong x = fixedPoint (\y -> average y (average y (x / y^4))) 1
-- fifthRootWrong x = fixedPoint (\y -> 0.5 * (y + 0.5 * (y + (x / y^4)))) 1
-- fifthRootWrong x = fixedPoint (\y -> 0.5 * y + 0.25 * (y + (x / y^4))) 1
-- fifthRootWrong x = fixedPoint (\y -> 0.75 * y + 0.25 * (x / y^4)) 1
--
--  n'th root case with m averageDamps would be:
--      fixedPoint (repeated averageDamp m (\y -> x / y^(n-1))) 1
--      fixedPoint (\y -> (1 - 1/2^m) * y + x / (2^m * y^(n-1))) 1
-- with m = log2 n:
--      fixedPoint (\y -> (1 - 1/n) * y + x / (n * y^(n-1))) 1

-- fifthRootRight x = fixedPoint (repeated (averageDamp (\y -> x / y^4)) 2) 1
-- fifthRootRight x = fixedPoint (averageDamp (\y -> x / y^4) . averageDamp (\y -> x / y^4)) 1
-- -- In this one (misnamed; it's the wrong one), removing the function-doubling
-- -- reveals that the sequence converges to a period-2 oscillating cycle, which
-- -- is masked by the doubling - making it falsely converge to one of the
-- -- 2 elements of the cycle.

-- newton-raphson iteration for finding root of
--      f  y = y^n - x
--      f' y = n*y^(n-1)
-- 
-- y - f y / f' y   = y - (y^n - x) / (n * y^(n-1))
--                  = y - y^n / (n * y^(n-1)) + x / (n * y^(n-1))
--                  = y - y / n + x / (n * y^(n-1))
--                  = (1-1/n) * y + x / (n * y^(n-1))

rt n x = fixedPoint (\y -> y - f y / f' y) 1
    where
        f  y = y**n - x
        f' y = n * y ** (n-1)

rt' n x = iterate (\y -> (1-1/n) * y + x / (n * y^(n-1))) 1
    where
        infixr 8 ^
        (^) = (**)