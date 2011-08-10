{-# LANGUAGE GADTs, TypeFamilies #-}
module TypeExperiments.FMap1 where

import Data.GADT.Compare
import Data.GADT.Show

-- Can this be stated, by any means, as a newtype or stack of newtypes?
-- I doubt it.
-- Also, I'm sure there's a better name for this... ideas?
data FMap1 f g a where
    FMap1 :: !(g a) -> FMap1 f g (f a)

instance GEq g => GEq (FMap1 f g) where
    geq (FMap1 x) (FMap1 y) = 
        case geq x y of
            Just Refl   -> Just Refl
            Nothing     -> Nothing

-- this is a sort of approximation i guess...
newtype FMap1' f g x = FMap1' (forall a. x ~ f a => g a)

-- ... but can this GEq instance be written?
instance GEq g => GEq (FMap1' f g) where
    maybeEq (FMap1' x) (FMap1' y) eq ne = maybeEq (x :: f x) (y :: f y) eq ne
