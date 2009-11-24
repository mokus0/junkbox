#!runhaskell
{-
 -	"fac.hs"
 -	(c) 2007 James Cook
 -}

module Math.Fac where

-- static Peano constructors and numerals
-- 
-- These model natural numbers as an algebra consisting of 2
-- constructors: Zero represents zero, and Succ takes the representation
-- of one number and produces the representation of the next number.
-- Giusseppe Peano proved that this algebra gives a "universal" model of the
-- natural numbers.
--
-- This implementation is at the type level - Zero and Succ are types, not
-- values.

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three


-- dynamic representatives for static Peanos

zero  = undefined :: Zero
one   = undefined :: One
two   = undefined :: Two
three = undefined :: Three
four  = undefined :: Four


-- addition, a la Prolog
-- remember, this is addition of type-level numbers, not values.  So
-- the type of "add one two" is the same as the type of "three".

class Add a b c | a b -> c where
  add :: a -> b -> c
  
instance              Add  Zero    b  b
instance Add a b c => Add (Succ a) b (Succ c)


-- multiplication, a la Prolog
-- Again, type level.  The type of "mul two two" is the same type as "four".

class Mul a b c | a b -> c where
  mul :: a -> b -> c

instance                           Mul  Zero    b Zero
instance (Mul a b c, Add b c d) => Mul (Succ a) b d


-- factorial, a la Prolog
-- Types can get pretty big pretty fast.

class Fac a b | a -> b where
  fac :: a -> b

instance                                Fac  Zero    One
instance (Fac n k, Mul (Succ n) k m) => Fac (Succ n) m

-- try, for "instance" (sorry):
-- 
--     :t fac four
