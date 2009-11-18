{-
 -      ``pythagoras''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Pythagoras where

import Control.Monad
import Control.Monad.Instances

p :: (Floating a) => a -> a -> a
p = curry (sqrt . liftM2 (+) ((^2).fst) ((^2).snd))

-- what would the 'pythagorean theorem' look like in the context of types?
-- I think: (C,C) ~= Either (A,A) (B,B)
-- (where ~= indicates type isomorphism)
-- Then C could be considered a type-level measurement of some concept of
-- "distance" between the types A and B.
--
-- (note I'm going to ignore _|_, strictness, etc.)
--
-- Any examples?
-- Let A = B.  Then:
--      Either (A,A) (B,B)  ~= Either (A,A) (A,A)
--                          ~= (Bool,A,A)
--                          ~= ?
--
-- Let  A = (forall a. a -> a -> a -> a)
--      B = (forall a. a -> a -> a -> a -> a)
-- iso (Left (f,g)) =   ( ??
--                      , ??
--                      )
-- iso (Right (f,g)) =  ( ??
--                      , ??
--                      )
-- 
-- Essentially, this isomorphism (assuming it can be constructed, which 
-- intuitively seems true based on the outline of the contsruction I am
-- about to suggest) proceeds by interpreting the pairs (A,A) and (B,B) as
-- 2-digit numbers (in base A and base B respectively) and mapping them
-- to a 2-digit number (in base C).  The "Left" case maps numbers in (A,A)
-- to numbers in the bottom part of (C,C).  The "Right" case maps (B,B) to
-- the "top" part of the range.  By the original value-level pythagorean 
-- theorem, the cardinalities match (meaning that the type-level construction
-- will always be "thinkable", but not necessarily constructible).