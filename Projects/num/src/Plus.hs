{-# LANGUAGE
    NoImplicitPrelude, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances
  #-}
module Plus where

import qualified Prelude as P
import Prelude ()

class Add a b c | a b -> c, a c -> b, b c -> a where
    (+) :: a -> b -> c

class Sub a b c | a b -> c, a c -> b, b c -> a where
    (-) :: a -> b -> c

newtype Time     t = Time     t deriving (P.Eq, P.Ord, P.Show)
newtype TimeSpan t = TimeSpan t deriving (P.Eq, P.Ord, P.Show)

instance Add P.Integer P.Integer P.Integer where
    (+) = (P.+)

instance Sub P.Integer P.Integer P.Integer where
    (-) = (P.-)

instance Add a b c => Add (Time a) (TimeSpan b) (Time c) where
    Time a + TimeSpan b = Time (a+b)

instance Add a b c => Add (TimeSpan a) (Time b) (Time c) where
    TimeSpan a + Time b = Time (a+b)

instance Add a b c => Add (TimeSpan a) (TimeSpan b) (TimeSpan c) where
    TimeSpan a + TimeSpan b = TimeSpan (a+b)

instance Sub a b c => Sub (Time a) (Time b) (TimeSpan c) where
    Time a - Time b = TimeSpan (a-b)

instance Sub a b c => Sub (Time a) (TimeSpan b) (Time c) where
    Time a - TimeSpan b = Time (a-b)

instance Sub a b c => Sub (TimeSpan a) (TimeSpan b) (TimeSpan c) where
    TimeSpan a - TimeSpan b = TimeSpan (a-b)
