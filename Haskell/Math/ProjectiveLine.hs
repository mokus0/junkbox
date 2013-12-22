module Math.ProjectiveLine where

data ProjectiveLine a
    = Real !a
    | Infinity
    deriving (Eq, Ord)

instance Show a => Show (ProjectiveLine a) where
    showsPrec p (Real x) = showsPrec p x
    showsPrec p Infinity = showString "Infinity"

instance Num a => Num (ProjectiveLine a) where
    fromInteger = Real . fromInteger
    Real x + Real y = Real (x + y)
    _      + _      = Infinity
    Real x - Real y = Real (x - y)
    _      - _      = Infinity
    Real x * Real y = Real (x * y)
    _      * _      = Infinity
    negate (Real x) = Real (negate x)
    negate Infinity = Infinity
    abs    (Real x) = Real (abs x)
    abs    Infinity = Infinity
    signum (Real x) = Real (signum x)
    signum Infinity = Infinity

instance (Fractional a, Eq a) => Fractional (ProjectiveLine a) where
    fromRational = Real . fromRational
    recip (Real 0) = Infinity
    recip (Real x) = Real (recip x)
    recip Infinity = Real 0

