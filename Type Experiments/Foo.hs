{-
 -      ``Foo''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    NoMonoPatBinds, NoMonomorphismRestriction, ImplicitParams
  #-}

module Foo where

class Foo x where
    foo :: Either x String -> (x, String)

eep :: (Foo x, ?foo :: Either x String) => (x, String)
eep@(x, bar) = foo ?foo

instance Foo Int where
    foo (Left i) = (i, show i)
    foo (Right s) = (read s, s)