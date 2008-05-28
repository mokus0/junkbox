{-
 -      ``Group2.hs''
 -      (c) 2008 James Cook
 -}

module Group2 where

import Data.Ratio

-- one 'sort', |Gen a|
-- 3 operations, |Unit|, |Inv|, and |(:*:)|
data Group a
        = Unit
        | Gen a
        | Inv (Group a)
        | Group a :*: Group a
        deriving (Eq, Ord, Show)

infixr 7 :*:

apply u lit inv (*) Unit        = u
apply u lit inv (*) (Gen a)     = lit a
apply u lit inv (*) (Inv x)     = inv (apply u lit inv (*) x)
apply u lit inv (*) (x :*: y)   = apply u lit inv (*) x * apply u lit inv (*) y

apply' :: (Integral a) => Group a -> Ratio a
apply' = apply 1 (%1) recip (*)

class (Eq a) => Normalizing a where
        reduce :: a -> a
        normalize :: a -> a
        normalize x
                | reduce x == x         = x
                | otherwise             = normalize (reduce x)

-- special order to support commutative groups: Inv becomes no-op wrt ordering,
-- so that inverses get sorted to the same location as the things they should
-- annihilate.  The product case is not quite right, but the way this is
-- used it doesn't need to be
(>*) :: (Ord a) => Group a -> Group a -> Bool
(>*) = apply (const False) genCmp id (\x y -> const False)
        where
                genCmp x = apply True (x >) id (\x y -> False)

-- laws:
instance (Eq a) => Normalizing (Group a) where
        -- associativity
        reduce ((x :*: y) :*: z)
                                = x :*: (y :*: z)
        
        -- distribution of inverses over the operation
        reduce (Inv (x :*: y))  = Inv y :*: Inv x
        
        -- commutativity (optional - requires Ord a)
        --reduce (x :*: y :*: z)
        --        | x >* y         = y :*: x :*: z
        --        | y >* z         = x :*: z :*: y
        
        -- inverses
        reduce (Inv (Inv x))    = x
        
        -- this is twice as many cases than I would like, but separate
        -- cases are necessary to handle inverses inside and at
        -- the ends of the expression
        reduce (Inv (Gen x) :*: Gen y)
                | x == y        = Unit
        reduce (Gen x :*: (Inv (Gen y)))
                | x == y        = Unit
        reduce (Gen x :*: Inv (Gen y) :*: z)
                | x == y        = z
        reduce (Inv (Gen x) :*: Gen y :*: z)
                | x == y        = z
        
        -- identity
        reduce (Inv Unit)       = Unit
        reduce (Unit :*: x)     = x
        reduce (x :*: Unit)     = x
        
        -- everything else unconstrained
        reduce Unit             = Unit
        reduce (Gen a)          = Gen a
        reduce (Inv x)          = Inv (reduce x)
        reduce (x :*: y)        = reduce x :*: reduce y

instance (Num a) => Num (Group a) where
        fromInteger 0 = Unit
        fromInteger x = Gen (fromInteger x)
        (+) = (:*:)
        negate = Inv
        
        (*) = error "Group doesn't define (*). Try (+)."
        abs = error "Group doesn't define abs."
        signum = error "Group doesn't define signum."