{-
 -	"fib.hs"
 -	(c) 2007 James Cook
 -}

module Fib where

-- I would like to create a program to take a haskell definition, e.g.:
    -- fib = 1 : 1 : zipWith (+) fib (tail fib)
-- and separate it into 2 parts, like these:
-- (ideally also implemented in a way that would allow manipulation of
-- the "syntax graph," including type inference, and flexible generation
-- of identifiers)

-- content
fib = a (:) 1 (:) 1 zipWith (+) fib tail fib

-- structure
a b c d e f g h i j = b c (d e (f g h (i j)))

-- self-reference operator
y f = f (y f)

-- random junk
-- a (:) 1 replicate 5 (foldl) (\x y -> sum (take x y)) 4 (\x -> [[a+b | a <- [1..x]] | b <- [1..x]]) 6