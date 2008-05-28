{-# OPTIONS 
        -fglasgow-exts 
        -fallow-undecidable-instances 
        -fallow-overlapping-instances 
  #-}
{-
 -      ``TypeEq.hs''
 -      (c) 2008 James Cook
 -}

module TypeEq where

class TypeEq a b where
        typeEq :: a -> b -> Bool

instance TypeEq a a where
        typeEq _ _ = True

instance TypeEq a b where
        typeEq _ _ = False