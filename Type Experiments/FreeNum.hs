{-
 -      ``FreeNum''
 -      (c) 2008 James Cook
 -}

module FreeNum where

data FreeNum a
        = Var a
        | FreeNum a :+: FreeNum a
        | FreeNum a :-: FreeNum a
        | FreeNum a :*: FreeNum a
        | Negate (FreeNum a)
        | Abs (FreeNum a)
        | Signum (FreeNum a)
        | FromInteger Integer
        deriving (Eq, Ord, Show)

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:

instance (Eq a, Show a) => Num (FreeNum a) where
        (+)             = (:+:)
        (-)             = (:-:)
        (*)             = (:*:)
        negate          = Negate
        abs             = Abs
        signum          = Signum
        fromInteger     = FromInteger

--reduceBy :: [FreeNum -> [FreeNum]] -> FreeNum -> [FreeNum]
reduceBy laws thing = do
        let recurse thing = case thing of
                Var x           -> []
                x :+: y         -> [x :+: y | x <- reduceBy laws x] ++ [x :+: y | y <- reduceBy laws y]
                x :-: y         -> [x :-: y | x <- reduceBy laws x] ++ [x :-: y | y <- reduceBy laws y]
                x :*: y         -> [x :*: y | x <- reduceBy laws x] ++ [x :*: y | y <- reduceBy laws y]
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