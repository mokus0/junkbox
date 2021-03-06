{-
 -      ``Foo''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    NoMonoPatBinds, NoMonomorphismRestriction, ImplicitParams
  #-}

module TypeExperiments.Foo where

class Foo x where
    foo :: Either x String -> (x, String)

eep :: (Foo x, ?foo :: Either x String) => (x, String)
eep@(x, bar) = foo ?foo

instance Foo Int where
    foo (Left i) = (i, show i)
    foo (Right s) = (read s, s)

-- not allowed (the instance):
-- class Bar x where
--     aoeu :: x -> String
-- 
-- instance (?bar :: String) => Bar Int where
--     aoeu i = ?bar