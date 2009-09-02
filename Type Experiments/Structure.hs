{-# LANGUAGE
        TypeFamilies,
        ScopedTypeVariables,
        TypeSynonymInstances
  #-}
{-
 - Structured data types parameterized by strictness...
 -}
  
module Structure where

import Data.Unamb

newtype Mu t = Mu (t (Mu t))

class Structured t where
    data Structure t :: (* -> *) -> * -> *
    compose   :: Strictness s => Struct s t -> t
    decompose :: Strictness s => t -> Struct s t
    recompose :: (Strictness s1, Strictness s2) => Struct s1 t -> Struct s2 t
    recompose = decompose . compose

instance Structured (Mu t) where
    newtype Structure (Mu t) s x = MuS (Mu t)
    compose (Mu (MuS t)) = t
    decompose = Mu . MuS
    -- ??

instance Structured Int where
    newtype Structure Int s x = Int (s Int)
    compose (Mu (Int x)) = unwrap x
    decompose x = Mu (Int (wrap x))
    recompose (Mu (Int x)) = Mu (Int (rewrap x))

instance Structured Integer where
    newtype Structure Integer s x = Integer (s Integer)
    compose (Mu (Integer x)) = unwrap x
    decompose x = Mu (Integer (wrap x))

instance Structured Char where
    newtype Structure Char s x = Char (s Char)
    compose (Mu (Char x)) = unwrap x
    decompose x = Mu (Char (wrap x))

instance (Show a, Structured a, Strictness s) => Show (Struct s a) where
    showsPrec p = showsPrec p . compose

instance Structured a => Structured [a] where
    data Structure [a] s x = Nil | Cons !(s (Struct s a)) !(s x)
    compose (Mu Nil) = []
    compose (Mu (Cons x xs)) = compose (unwrap x) : (compose (unwrap xs))
    decompose [] = Mu Nil
    decompose (x:xs) = Mu (Cons (wrap (decompose x)) (wrap (decompose xs)))

instance (Structured a, Structured b) => Structured (Either a b) where
    data Structure (Either a b) s x = LeftS !(s (Struct s a)) | RightS !(s (Struct s b))
    compose (Mu (LeftS  a)) = Left  (compose (unwrap a))
    compose (Mu (RightS a)) = Right (compose (unwrap a))
    decompose (Left  a) = Mu (LeftS  (wrap (decompose a)))
    decompose (Right a) = Mu (RightS (wrap (decompose a)))

eval :: Structured t => t -> t
eval (x :: t) = compose (decompose x :: Struct Strict t)

strictify :: (Structured a, Strictness s) => Struct s a -> Struct Strict a
strictify = recompose

type Struct s t = Mu (Structure t s)

newtype Strict a = Strict a
data Lazy a = Lazy a
data Mixed a 
    = Str !a
    | Lzy a

class Strictness t where
    wrap   :: Structured a => a -> t a
    unwrap :: Structured a => t a -> a

instance Strictness Strict where
    wrap = Strict
    unwrap (Strict x) = x

instance Strictness Lazy where
    wrap = Lazy
    unwrap (Lazy x) = x

instance Strictness Mixed where
    wrap x = unamb (Lzy x) (x' `seq` Str x')
        where x' = eval x
    unwrap (Str a) = a
    unwrap (Lzy a) = a

rewrap x = wrap (unwrap x)