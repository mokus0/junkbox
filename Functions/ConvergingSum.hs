{-
 -      ``ConvergingSum''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}

module ConvergingSum where

convergingSum z xs = go xs z
    where
        go [] y = y
        go (x:xs) y
            | (x + y) == y    = y
            | otherwise     = go xs $! x+y

convergingSumC z xs = go xs z
    where
        go [] y = (y, True)
        go (x:xs) y
            | (x + y) == y    = (y, False)
            | otherwise     = go xs $! x+y

convergenceSteps z xs = go xs z 0
    where
        go [] y n = n
        go (x:xs) y n
            | (x + y) == y    = n
            | otherwise     = (go xs $! x+y) $! n+1

