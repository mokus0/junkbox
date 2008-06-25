{-
 -      ``FreeNum''
 -      (c) 2008 James Cook
 -}

module FreeNum where

default (FreeNum)

x = sum [1..10]

data FreeNum
        = FreeNum :+: FreeNum
        | FreeNum :-: FreeNum
        | FreeNum :*: FreeNum
        | Negate FreeNum
        | Abs FreeNum
        | Signum FreeNum
        | FromInteger Integer
        deriving (Eq, Ord, Show)

foldNum (+) (-) (*) negate abs signum fromInteger num = case num of
        (a :+: b)       -> f a + f b
        (a :-: b)       -> f a - f b
        (a :*: b)       -> f a * f b
        Negate a        -> negate (f a)
        Abs a           -> abs (f a)
        Signum a        -> signum (f a)
        FromInteger a   -> fromInteger a
        where f = foldNum (+) (-) (*) negate abs signum fromInteger

toNum :: Num a => FreeNum -> a
toNum = foldNum (+) (-) (*) negate abs signum fromInteger 

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:

instance Num FreeNum where
        (+)             = (:+:)
        (-)             = (:-:)
        (*)             = (:*:)
        negate          = Negate
        abs             = Abs
        signum          = Signum
        fromInteger     = FromInteger

instance Enum FreeNum where
        toEnum = fromInteger . toEnum
        fromEnum = fromEnum . toNum

--reduceBy :: [FreeNum -> [FreeNum]] -> FreeNum -> [FreeNum]
reduceBy laws thing = do
        let recurse thing = case thing of
                x :+: y         -> [x :+: y  | x <- reduceBy laws x] ++ [x :+: y | y <- reduceBy laws y]
                x :-: y         -> [x :-: y  | x <- reduceBy laws x] ++ [x :-: y | y <- reduceBy laws y]
                x :*: y         -> [x :*: y  | x <- reduceBy laws x] ++ [x :*: y | y <- reduceBy laws y]
                Negate x        -> [Negate x | x <- reduceBy laws x]
                Abs x           -> [Abs    x | x <- reduceBy laws x]
                Signum x        -> [Signum x | x <- reduceBy laws x]
                FromInteger x   -> []
        law <- laws ++ [recurse]
        reduced <- law thing
        reduced : reduceBy laws reduced

normalizeBy laws thing = last (thing : reduceBy laws thing)

--
-- some laws to experiment with.
--

preservesAddition (FromInteger x :+: FromInteger y)     = [FromInteger (x+y)]
preservesAddition _                                     = []

preservesSubtraction (FromInteger x :-: FromInteger y)  = [FromInteger (x-y)]
preservesSubtraction _                                  = []

-- these next two interact badly... their normal forms can be mutually-reducible
associativeAddition ((x :+: y) :+: z)                   = [x :+: (y :+: z)]
associativeAddition _                                   = []

commutativeAddition (x :+: y)
        | x > y                                         = [y :+: x]
commutativeAddition _                                   = []

negateIsAdditiveInverseWithUnit one (x :+: Negate y)
        | x == y                                        = [one]
negateIsAdditiveInverseWithUnit one (Negate x :+: y)
        | x == y                                        = [one]
negateIsAdditiveInverseWithUnit _ _                     = []

subtractionIsNegatedAddition (x :-: y)                  = [x :+: Negate y]
subtractionIsNegatedAddition _                          = []

preservesNegation (Negate (FromInteger x))              = [FromInteger (negate x)]
preservesNegation _                                     = []

multiplicationDistributes ((x :+: y) :*: z)             = [(x :*: z) + (y :*: z)]
multiplicationDistributes (z :*: (x :+: y))             = [(z :*: x) + (z :*: y)]
multiplicationDistributes _                             = []

preservesMultiplication (FromInteger x :*: FromInteger y) = [FromInteger (x*y)]
preservesMultiplication _                               = []

negationDistributesOverAddition (Negate (x :+: y))      = [Negate x :+: Negate y]
negationDistributesOverAddition _                       = []

negationFactorsFromMultiplication (Negate x :*: y)      = [Negate (x :*: y)]
negationFactorsFromMultiplication (x :*: Negate y)      = [Negate (x :*: y)]
negationFactorsFromMultiplication _                     = []

negationFoldsIntoMultiplication (Negate (x:*:y))        = [Negate x :*: y, x :*: Negate y]
negationFoldsIntoMultiplication _                       = []