{-
 -      ``OpenFold''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module TypeExperiments.OpenFold where

-- consider 'extensible case analysis' as a folding operation.
-- 
-- start with the following "extension" of the type "Either":
-- (expressed in 2 ways)

data E3 a b c = E (Either a c) | Center b
        deriving (Eq, Show)
data Either3 a b c = L a | R b | C c
        deriving (Eq, Show)

-- From these, I wish to ("elegantly") define a fold operation which
-- can simultaneously take the types:
--   fold :: (a -> t) -> (b -> t) -> Either a b -> t
--   fold :: (a -> t) -> (b -> t) -> (c -> t) -> E3 a b c -> t
--   fold :: (Either a b -> t) -> (c -> t) -> E3 a b c -> t


-- And here's a slightly more complicated version of the same notion:
--
-- consider extending a recursive type:

data Foo a = Nil | Cons a (Foo a) | Join (Foo a) (Foo a)
        deriving (Eq, Show)

-- Considering this as an "extension" of [a], let fold simultaneously take the types:
--   fold :: t -> (a -> t -> t) -> [a] -> t
--   fold :: t -> (a -> t -> t) -> (t -> t -> t) -> Foo a -> t
--   fold :: ([a] -> t) -> (t -> t -> t) -> Foo a -> t

-- It would also be nice to allow application of functions on the 'extension' to the base type
--   ($) :: (E3 a b c -> t) -> (Either a b) -> t
--   ($) :: (Foo a -> t) -> [a] -> t

-- and some sort of constructor overloading:
--   left :: a -> Either a b
--   left :: a -> E3 a b c
--
-- though the need for this is alleviated by the ability to
-- easily extend the functions using the generated values (that is,
-- why bother overloading Either's constructors when you can already fold
-- it as if it were an instance of E3?)