{-
 -      ``rec''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Rec where

newtype Thing a = Thing {thing :: a}
    deriving (Eq, Show)