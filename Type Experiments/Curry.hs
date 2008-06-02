{-
 -      ``Curry''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeFamilies,
        FlexibleInstances,
        FlexibleContexts,
        MultiParamTypeClasses,
        FunctionalDependencies,
        UndecidableInstances,
        OverlappingInstances,
        IncoherentInstances,
        RankNTypes
  #-}

module Curry where

import Prelude hiding (curry, uncurry, ($))
import Control.Arrow
import qualified Data.Map as M

-- objective here: write a very generic 'curry' operation, which 'flattens' such types as:
-- (a,b) -> c
-- (a,b,c) -> (d,e) -> f
-- a -> (b, c) -> e

-- a corresponding "uncurry" would be a nice bonus, but I suspect it would be even harder.

-- I'm also using this as a scratch pad for some other stuff while learning to use type families.

class Apply f where
        type Antecedent f
        type Consequent f
        ($) :: f -> Antecedent f -> Consequent f

class ( Apply f
      , Apply g
      , Consequent f ~ Antecedent g
      , Apply (Composed f g)
      
      , Antecedent f ~ Antecedent (Composed f g)
      , Consequent g ~ Consequent (Composed f g)
      ) => Compose f g where
        
        type Composed f g
        (#) :: f -> g -> Composed f g

instance Apply (a -> b) where
        type Antecedent (a -> b) = a
        type Consequent (a -> b) = b
        f $ x = f x

instance (Ord k) => Apply (M.Map k v) where
        type Antecedent (M.Map k v) = k
        type Consequent (M.Map k v) = Maybe v
        ($) = flip M.lookup

instance Monad m => Apply (Kleisli m a b) where
        type Antecedent (Kleisli m a b) = m a
        type Consequent (Kleisli m a b) = m b
        f $ x = x >>= runKleisli f

instance (Apply f, Apply g, Consequent f ~ Antecedent g) => Compose f g where
        type Composed f g = Antecedent f -> Consequent g
        f # g = \x -> g $ (f $ x)



class Coalesce a where
        type Coalesced a
        coalesce :: a -> Coalesced a

instance Coalesce ((a,b), (c, d)) where
        type Coalesced ((a,b), (c,d)) = (a,b,c,d)
        coalesce ((a,b),(c,d)) = (a,b,c,d)

instance Coalesce ((a,b,c), (d,e)) where
        type Coalesced ((a,b,c), (d,e)) = (a,b,c,d,e)
        coalesce ((a,b,c), (d,e)) = (a,b,c,d,e)

instance Coalesce ((a,b), (c,d,e)) where
        type Coalesced ((a,b), (c,d,e)) = (a,b,c,d,e)
        coalesce ((a,b), (c,d,e)) = (a,b,c,d,e)

class Curry a where
        type Curried a
        curry :: a -> Curried a
        uncurry :: Curried a -> a

instance Curry ((a,b) -> c) where
        type Curried ((a,b) -> c) = a -> b -> c
        curry f a b = f (a,b)
        uncurry f (a,b) = f a b

instance Curry ((a,b,c) -> d) where
        type Curried ((a,b,c) -> d) = a -> b -> c -> d
        curry f a b c = f (a,b,c)
        uncurry f (a,b,c) = f a b c

instance Curry ((a,b,c,d) -> e) where
        type Curried ((a,b,c,d) -> e) = a -> b -> c -> d -> e
        curry f a b c d = f (a,b,c,d)
        uncurry f (a,b,c,d) = f a b c d

-- bork!
-- instance (Curry c) => Curry ((a,b) -> c) where
--         type Curried ((a,b) -> c) = a -> b -> Curried c
--         

class Flatten a b where
        flatten :: a -> b

-- base case
instance Flatten a a where
        flatten = id

-- pairs
instance (Flatten a (b, c)) => Flatten (a,d) (b,c,d) where
        flatten (a,d) = case flatten a of (b,c) -> (b,c,d)

instance (Flatten c (d, e)) => Flatten (a,c) (a,d,e) where
        flatten (a,c) = case flatten c of (d,e) -> (a,d,e)

instance (Flatten a (b,c), Flatten d (e,f)) => Flatten (a,d) (b,c,e,f) where
        flatten (a,d) = case (flatten a, flatten d) of
                ((b,c),(e,f)) -> (b,c,e,f)
        
-- I suspect similar problems lurk here
class Curry2 a b where
        curry2 :: a -> b
        uncurry2 :: b -> a

instance Curry2 ((a,b) -> c) (a -> b -> c) where
        curry2 f a b = f (a,b)
        uncurry2 f (a,b) = f a b

instance Curry2 ((a,b,c) -> d) (a -> b -> c -> d) where
        curry2 f a b c = f (a,b,c)
        uncurry2 f (a,b,c) = f a b c

instance Curry2 ((a,b,c,d) -> e) (a -> b -> c -> d -> e) where
        curry2 f a b c d = f (a,b,c,d)
        uncurry2 f (a,b,c,d) = f a b c d

instance (Curry2 b c) => Curry2 (a -> b) (a -> c) where
        curry2   f x = curry2   (f x)
        uncurry2 f x = uncurry2 (f x)