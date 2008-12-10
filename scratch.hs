{-
 -      ``scratch''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        GeneralizedNewtypeDeriving
  #-}

module Scratch where

f 4 = "four"
f 5 = "eep"

f' :: Integral a => a -> String
f' 4 = "four"
f' 5 = "eep"

g 0 = "zero"
g (n+1) = g n ++ " + 1"

newtype FooNum = Foo Int deriving (Num, Show)

instance Eq FooNum where
        (==) = error "=="
        (/=) = error "/="
instance Ord FooNum where
        compare = error "compare"
        (>)  = error ">"
        (>=) = error ">="
        (<)  = error "<"
        (<=) = error "<="
        