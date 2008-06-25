{-
 -      ``Apply2''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeFamilies,
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts,
        UndecidableInstances,
        RankNTypes
  #-}

module Apply2 where

import Prelude hiding (($), (.), map, (>>=))
import qualified Prelude
import qualified Data.Map as M

newtype Set a           = Set {inSet :: a -> Bool}
newtype Relation a b    = Relation {inRelation :: a -> b -> Bool}

infixr 0 $
infixl 1 >>=
infixr 9 .

-- base definitions of application and composition
class Apply a b where
        type Result a b :: *
        ($) :: a -> b -> Result a b

data Compose a b = Compose a b
(.) :: a -> b -> Compose a b
(.) = Compose

--instance (Apply a b, Apply (Result a b) c) => Apply (Compose a b) c where
--        type Result (Compose a b) c = Result (Result a b) c
--        Compose f g $ x = f $ (g $ x)
instance (Apply b c, Apply a (Result b c)) => Apply (Compose a b) c where
        type Result (Compose a b) c = Result a (Result b c)
        Compose f g $ x = f $ (g $ x)

class ( Apply f x
      , Apply (Inverse f) (Result f x)
      , Result (Inverse f) (Result f x) ~ x
      ) => Invertible f x where
        type Inverse f :: *
        invert :: f -> Inverse f

instance (a ~ c) => Apply (a -> b) c where
        type Result (a -> b) c = b
        f $ x = f x

-- can't use this in conjunction with the next instance (and this instance can't be made "invertible"):
--instance (a ~ c) => Apply (Relation a b) c where
--        type Result (Relation a b) c = Set b
--        (Relation r) $ x = Set (r x)

class Existential c where
        exists :: (c -> Bool) -> Bool

instance (a ~ c, Existential c) => Apply (Relation a b) (Set c) where
        type Result (Relation a b) (Set c) = Set b
        (Relation r) $ (Set s) = Set (\y -> exists (\x -> s x && r x y))

-- this seems right, but it panics ghc (6.9.20080602):
--instance (a ~ c, Existential a, Existential b) => Invertible (Relation a b) (Set c) where
--        type Inverse (Relation a b) = Relation b a
--        invert (Relation r) = Relation (flip r)

-- problematic: this should be able to have a simple polymorphic return type (Monad m => m v)
instance (Ord k, k ~ k') => Apply (M.Map k v) k' where
        type Result (M.Map k v) k' = Maybe v
        f $ x = M.lookup x f

-- in general, partial functions are problematic at the type level...
--instance (k ~ k') => Invertible (M.Map k v) k' where
--        type Inverse (M.Map k v) = M.Map v k
--        invert m = M.fromList (map swap (M.toList m))
--                where swap = uncurry (flip (,))

newtype Map f = Map f
map f x = Map f $ x

instance (Functor f, Apply a b) => Apply (Map a) (f b) where
        type Result (Map a) (f b) = f (Result a b)
        Map f $ xs = fmap (f $) xs

newtype Bind x = Bind x
x >>= f         = Bind f $ x

instance (Monad m, Apply a b, Result a b ~ m c) => Apply (Bind a) (m b) where
        type Result (Bind a) (m b) = Result a b
        Bind f $ x = x Prelude.>>= (f $)
