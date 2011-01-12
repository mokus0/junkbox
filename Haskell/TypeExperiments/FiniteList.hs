module TypeExperiments.FiniteList where

data FiniteList a
    = Nil
    | Cons a !(FiniteList a)
    deriving (Eq, Show)