{-
 -      ``pythagoras''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Pythagoras where

import Control.Monad
import Control.Monad.Instances

p :: (Floating a) => a -> a -> a
p = curry (sqrt . liftM2 (+) ((^2).fst) ((^2).snd))