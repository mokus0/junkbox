module TypeExperiments.Scratch where

import Prelude hiding (id, (.))
import Control.Category

data C a b = C deriving (Eq, Show)
instance Category C where
    id = C
    C . C = C

data A = A
y :: C a (A -> a)
y = C

