{-
 -      ``InfKind''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE
    KindSignatures,
    EmptyDataDecls
  #-}

module TypeExperiments.InfKind where

data Fix s a = In {out :: s a (Fix s a)}

data Foo a b c = Foo (a b) c