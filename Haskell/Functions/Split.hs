{-
 -      ``Split''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Split where

mapTail f = liftM2 (:) head (map f . tail)

splitOn p str = mapTail tail (groupBy (const (not.p)) str)

