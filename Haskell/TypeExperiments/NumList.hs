module TypeExperiments.NumList where
import Data.Void
import Data.List (genericLength)

-- silly proof-of-concept

-- |class characterizing unit type, so that our instance below can be
-- Haskell98 compatible ("instance Num [Void]" wouldn't be)
class Unit u where
    -- |The one value of the type
    unit :: u

instance Unit Void where
    unit = undefined

instance Eq Void where
    _ == _ = True

instance Ord Void where
    compare _ _ = EQ

instance Show Void where
    showsPrec p _ = showString "⊥"

instance (Unit t, Eq t, Show t) => Num [t] where
    fromInteger n
        | n <= 0    = []
        | otherwise = unit : fromInteger (n-1)
    
    (+) = (++)
    
    [] -  _ = []
    xs - [] = xs
    (_:xs) - (_:ys) = xs - ys
    
    (*) = (>>)
    
    abs = id
    signum [] = []
    signum _  = 1

instance Unit t => Enum [t] where
    fromEnum = length
    toEnum = flip replicate undefined
    succ = (unit:)
    pred [] = error "pred: bad arg"
    pred (_:xs) = xs

instance (Unit t, Ord t, Show t) => Real [t] where
    toRational = fromInteger . genericLength

instance (Unit t, Ord t, Show t) => Integral [t] where
    -- could make quot and div lazier, as well as first component of quotRem result
    divMod = quotRem
    quotRem a 0 = error "divide by zero"
    quotRem x y = go 0 x
        where
            go q 0 = (q, 0)
            go q r = case cmpSub r y of
                Just diff  -> go (unit:q) diff
                Nothing    -> (q, r)
    
    toInteger = genericLength

-- @cmpSub x y@: if x >= y, returns @Just (x-y)@, otherwise @Nothing@

cmpSub    xs     []  = Just xs
cmpSub    []      _  = Nothing
cmpSub (_:xs) (_:ys) = cmpSub xs ys

f = id :: [Void] -> [Void]
