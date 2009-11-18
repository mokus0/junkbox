{-# LANGUAGE
        GADTs, TemplateHaskell
  #-}
module GADT_Fold where

import Language.Haskell.TH.Fold

data Foo a where
    Foo1 :: a    -> Foo a
    Foo2 :: [a]  -> Foo a

foldFoo = $(fold ''Foo)

data Bar f g a where
    Bar1 :: f a -> Bar f g a
    Bar2 :: g a -> Bar f g a

foldBar = $(fold ''Bar)