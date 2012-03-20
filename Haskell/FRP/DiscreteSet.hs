module FRP.DiscreteSet where

import qualified Data.Set as S
import Reactive.Banana

data SetDelta a = SetDelta
    { inserted  :: S.Set a
    , removed   :: S.Set a
    } deriving (Eq, Ord, Read, Show)

applyDelta (SetDelta ins del) = S.union ins . flip S.difference del
computeDelta s1 s2 = SetDelta (s2 S.\\ s1) (s1 S.\\ s2)

data DiscreteSet t a = DiscreteSet 
    { setDeltas     :: Event t (SetDelta a)
    , discreteSet   :: Discrete t (S.Set a)
    }

-- does not enforce sanity of the deltas...
accumSet :: Ord a => Event t (SetDelta a) -> DiscreteSet t a
accumSet deltas = DiscreteSet deltas (accumD S.empty (applyDelta <$> deltas))

discretizeSet :: Ord a => Event t (S.Set a) -> DiscreteSet t a
discretizeSet sets = DiscreteSet deltas setsD
    where
        setsD = stepperD S.empty sets
        deltas = uncurry computeDelta <$> pairs (Just (initial setsD)) (changes setsD)

pairs :: Maybe a -> Event t a -> Event t (a, a)
pairs mbStart e = filterJust (uncurry (liftA2 (,)) <$> accumE (Nothing, mbStart) (step <$> e))
    where
        step newer (old, new) = (new, Just newer)

