module NewtypeInlining where

import Unsafe.Coerce

newtype Foo a = Foo a deriving (Eq, Show)

data FunctOr a
    = Nil
    | Cons a (FunctOr a)
    | WhoKnows a [Either a (FunctOr [a])]
    deriving (Eq, Show)

instance Functor FunctOr where
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)
    fmap f (WhoKnows a b) = WhoKnows (f a) (map (either (Left . f) (Right . fmap (map f))) b)

foo :: FunctOr a -> FunctOr (Foo a)
foo = fmap Foo

unFoo :: FunctOr (Foo a) -> FunctOr a
unFoo = fmap (\(Foo a) -> a)

-- equivalent to foo, by identical representation of (Foo a) ~ a
bar :: FunctOr a -> FunctOr (Foo a)
bar = unsafeCoerce

unBar :: FunctOr (Foo a) -> FunctOr a
unBar = unsafeCoerce

-- equivalent to id (once types are erased; unsafeCoerce before that)
eep :: a -> Foo a
eep = Foo

-- question - does foo automatically become bar when compiling?
-- what about unFoo?  eep?