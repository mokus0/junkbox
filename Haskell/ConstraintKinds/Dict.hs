{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes #-}
module ConstraintKinds.Dict
    (Dict(..), eqDict) where

import Data.Typeable

data Dict cxt where Dict :: cxt => Dict cxt

instance Eq   (Dict cxt) where Dict == Dict = True
instance Ord  (Dict cxt) where compare Dict Dict = EQ
instance Show (Dict cxt) where showsPrec _ Dict = showString "Dict"

-- 'Dict (a ~ b)' with the arguments in places suitable for 
-- manipulation with "gcast"
newtype EqDict a b = EqDict {getEqDict :: Dict (a ~ b)}

eqDict :: (Typeable a, Typeable b) => Maybe (Dict (a ~ b))
eqDict = fmap getEqDict (gcast (EqDict Dict))
