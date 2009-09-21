{-# LANGUAGE ImplicitParams, FlexibleInstances #-}
module TypeExperiments.ImplicitParams where

class Foo a where
    foo :: a -> String

-- why not allowed?
instance (?foo :: String) => Foo a where
    foo _ = ?foo