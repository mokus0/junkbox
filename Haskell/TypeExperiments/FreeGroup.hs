{-# OPTIONS -fglasgow-exts #-}
{-
 -      ``FreeGroup.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module TypeExperiments.FreeGroup where

data FreeGroup a
    = FreeGroup a :*: FreeGroup a
    | Inv (FreeGroup a)
    | Var a
    | Unit
    deriving (Eq, Show)

-- fold is a function which, given the operations associated with any
-- group algebra, produces the unique mapping from the free group
-- to that group.  For example, |fold (+) negate id 0| is the unique
-- morphism from the free group generated by the integers (Z)
-- to the group (Z, +).  Similarly, |fold (*) recip id 1| is the unique
-- morphism from the free group generated by the rationals (Q) to 
-- the group (Q, *).
fold :: (b -> b -> b) -> (b -> b) -> (a -> b) -> b -> FreeGroup a -> b
fold (*) inv var unit (x :*: y) = (fold (*) inv var unit x) * (fold (*) inv var unit y)
fold (*) inv var unit (Inv x)   = inv (fold (*) inv var unit x)
fold (*) inv var unit (Var x)    = var x
fold (*) inv var unit (Unit)    = unit

-- various normalization laws; the ``free group'' is not the free term algebra
-- of this data type; I'd like to experiment a bit and see whether it can be
-- expressed as a free algebra of some type or mutually recursive system of
-- types - or perhaps using a GADT?
reduce (Inv Unit) = Unit
reduce (Inv (Inv x)) = reduce x
reduce (Inv (x :*: y)) = reduce (reduce (Inv (reduce y)) :*: reduce (Inv (reduce x)))
reduce (Inv x) = Inv (reduce x)

reduce (Unit :*: x) = reduce x
reduce (x :*: Unit) = reduce x

reduce (Inv x :*: y)
    | x == y            = Unit
reduce (Inv x :*: (y :*: z))
    | x == y            = reduce z

reduce (x :*: Inv y)
    | x == y            = Unit
reduce (x :*: (Inv y :*: z))
    | x == y            = reduce z

reduce ((x :*: y) :*: z) = reduce (reduce x :*: reduce (reduce y :*: reduce z))
reduce (x :*: y) = reduce x :*: reduce y

reduce (Var x) = (Var x)
reduce Unit = Unit

instance (Eq a, Show a) => Num (FreeGroup a) where
    fromInteger 1 = Unit
    x * y = reduce (x :*: y)

instance (Eq a, Show a) => Fractional (FreeGroup a) where
    recip = reduce . Inv

x = Var "x"
y = Var "y"
z = Var "z"

-- commutators
c x y           = recip x * recip y * x * y
c2 x y z        = c z (x * y) * c y x

-- handy for working with many free-algebra monads
swap x y z 
        | x == y        = z     -- no-op when Eq is not strict equality
        | z == x        = y
        | z == y        = x
        | otherwise     = z

instance Functor FreeGroup where
    fmap f = fold (:*:) Inv (Var . f) Unit

instance Monad FreeGroup where
    return = Var
    x >>= f = (fold (:*:) Inv f Unit x)